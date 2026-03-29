//SETUPP4 JOB  (SETUP),                                               
//             'SETUP PRJ4',                                      
//             CLASS=A,                                               
//             MSGCLASS=X,                                            
//             MSGLEVEL=(0,0),                                        
//             NOTIFY=&SYSUID                                         
//********************************************************************
//*                                                                   
//* SETUP PRJ4 FILES IN EACH DATASET              
//*                                                                   
//********************************************************************
//* 
//*********************************************************************
//* DELETE PRIOR VERSIONS OF SOURCE AND OBJECT DATASETS               *
//*********************************************************************
//*                                                                    
//IDCAMS  EXEC PGM=IDCAMS,REGION=1024K                                 
//SYSPRINT DD  SYSOUT=*                                                
//SYSIN    DD  *                                                       
    DELETE PRJ4.DEV.BCOB NONVSAM SCRATCH PURGE                    
    DELETE PRJ4.DEV.COPYBOOK NONVSAM SCRATCH PURGE                    
    DELETE PRJ4.DEV.JCL NONVSAM SCRATCH PURGE                    
    SET MAXCC=0
/*                                                                     
//*                                                                    
//*********************************************************************
//* CREATE A PDS WITH PROGRAM SOURCE                                  *
//*********************************************************************
//*                                                                    
//STEP01 EXEC PGM=IEBUPDTE,REGION=1024K,PARM=NEW                      
//SYSPRINT DD  SYSOUT=*                                                 
//*                                                                     
//SYSUT2   DD  DSN=PRJ4.DEV.BCOB,DISP=(,CATLG,DELETE),             
//             UNIT=TSO,SPACE=(TRK,(15,,2)),                            
//             DCB=(RECFM=FB,LRECL=80,BLKSIZE=800)                      
//SYSPRINT DD  SYSOUT=*                                                 
//SYSIN    DD  DATA,DLM='><'                                            
./ ADD NAME=MYTAX,LIST=ALL                                            
      **************************************************************
      *
      *  PROGRAM ID MYTAX
      *  DATE CREATED:  21MAR2026
      *
      *  TAX RETURN / TAX FILING PROG 
      *  USING JAY JAY MOSELEY PROGRAM + CODE SNIPPETS
      *   DONT NEED TO DIRECTLY IMPORT VSAM IN FILE CONTROL, JAYS
      *  ASSEMBLER VSAMIO WILL PULL IT FOR YOU 
      *
      *  1  WE ARE ASSUMING ALL RECORDS IN SEQUENCTIAL RETURNS FILE
      *    ARE IN THE VSAM DATABASE  FOR NOW 
      *
      *  CHANGE LOG
      *  USER ID     DATE     CHANGE DESCRIPTION
      * ---------   ------    -------------------------------------
      *  DAN BIEG   21MAR2026 CODE PROG
      **************************************************************
       IDENTIFICATION DIVISION.   
      **************************************************************

       PROGRAM-ID. MYTAX.  

      **************************************************************
       ENVIRONMENT DIVISION.
      **************************************************************
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-370.
       OBJECT-COMPUTER. IBM-370. 

       INPUT-OUTPUT SECTION.

       FILE-CONTROL.            
           SELECT INPUT-RETURNS-SQ  ASSIGN TO UT-S-RETURNS.
           SELECT OUTPUT-REPORT-SQ  ASSIGN TO UT-S-REPORT.
           SELECT OUTPUT-DISPERSE-SQ  ASSIGN TO UT-S-DISPERSE.
           SELECT OUTPUT-AUDIT-SQ  ASSIGN TO UT-S-AUDIT.


      **************************************************************
       DATA DIVISION.       
      **************************************************************
       FILE SECTION. 

      * INPUT SEQ FILE:

       FD  INPUT-RETURNS-SQ        
           LABEL RECORDS ARE OMITTED            
           BLOCK CONTAINS 0 RECORDS      
           DATA RECORD IS FIL-INP-RETURNS. 
       
       01  FIL-INP-RETURNS                   PIC X(80).

      * -- -- --

      * OUTPUT SEQ FILES:

       FD  OUTPUT-REPORT-SQ        
           LABEL RECORDS ARE OMITTED            
           BLOCK CONTAINS 0 RECORDS      
           DATA RECORD IS FIL-OUT-REPORT. 
       
       01  FIL-OUT-REPORT                   PIC X(132).
      * DOUBLE CHECK THAT THE FILE WE CREATE IS BIG ENOUGHT TO HOLD THIS

      * -- 

       FD  OUTPUT-DISPERSE-SQ        
           LABEL RECORDS ARE OMITTED            
           BLOCK CONTAINS 0 RECORDS      
           DATA RECORD IS FIL-OUT-DISPERSE. 
       
       01  FIL-OUT-DISPERSE                   PIC X(80).

      * -- 

       FD  OUTPUT-AUDIT-SQ        
           LABEL RECORDS ARE OMITTED            
           BLOCK CONTAINS 0 RECORDS      
           DATA RECORD IS FIL-OUT-AUDIT. 
       
       01  FIL-OUT-AUDIT                   PIC X(80).

      **************************************************************
       WORKING-STORAGE SECTION. 
      **************************************************************

      * SEPERATORS AND MESSAGES
       01  WS-BREAKPT     PIC X(25) VALUE '-=-=-=-=-=-=-=-=-=-=-=-=-'.
       01  WS-MESSAGE     PIC X(25) VALUE 'BATCH TAX RETURN PROCESNG'.
       01  WS-LINE-SPACE  PIC X(25) VALUE SPACES.
       01  WS-FULL-SEPERATOR    PIC X(50) VALUE ALL '*'.

      * CURRENT DATE - FOR DEMO -- ASSUME 1978
       01  WS-CURRENT-DATE.
            02 WS-CURRENT-MM    PIC 9(2) VALUE 04.
            02 FILLER           PIC X(1) VALUE '-'.
            02 WS-CURRENT-DD    PIC 9(2) VALUE 17.
            02 FILLER           PIC X(1) VALUE '-'.
            02 WS-CURRENT-YY    PIC 9(4) VALUE 1978.


      * FLAGS
       01  WS-VAL.
           02 WS-EOF-RETURNS            PIC X VALUE 'N'.
           02  WS-AUDIT-FLAG            PIC X VALUE 'N'.
              88  NEEDS-AUDIT                 VALUE 'Y'.
           02  WS-MASTER-FOUND          PIC X VALUE 'N'.
              88  MASTER-FOUND                VALUE 'Y'.

      * COUNTERS
       01  WS-COUNTERS.
           05  WS-RETURNS-READ     PIC 9(06)  VALUE ZEROES.
           05  WS-RETURNS-PROC     PIC 9(06)  VALUE ZEROES.
           05  WS-RETURNS-ERR      PIC 9(06)  VALUE ZEROES.
           05  WS-REFUNDS-ISSUED   PIC 9(06)  VALUE ZEROES.
           05  WS-TAXES-OWED       PIC 9(06)  VALUE ZEROES.
           05  WS-AUDITS-FLAGGED   PIC 9(06)  VALUE ZEROES.
           05  WS-TOTAL-REFUNDS    PIC S9(09)V99 VALUE ZEROES.
           05  WS-TOTAL-COLLECTED  PIC S9(09)V99 VALUE ZEROES.

      * TAX CALC WORK FIELDS
       01  WS-TAX-CALC.
           05  WS-AGI              PIC S9(08)V99 VALUE ZEROES.
           05  WS-STD-DEDUCTION    PIC S9(07)V99 VALUE ZEROES.
           05  WS-TAXABLE-INCOME   PIC S9(08)V99 VALUE ZEROES.
           05  WS-GROSS-TAX        PIC S9(07)V99 VALUE ZEROES.
           05  WS-TOTAL-CREDITS    PIC S9(06)V99 VALUE ZEROES.
           05  WS-NET-TAX          PIC S9(07)V99 VALUE ZEROES.
           05  WS-BALANCE-DUE      PIC S9(07)V99 VALUE ZEROES.
           05  WS-EXEMPTION-AMT    PIC S9(06)V99 VALUE ZEROES.
           05  WS-DED-RATIO        PIC S9(03)V99 VALUE ZEROES.
           05  WS-WORK-AMT         PIC S9(09)V99 VALUE ZEROES.

      * STANDARD DEDUCTIONS
       01  WS-STD-DED-TABLE.
           05  WS-STD-DED-SINGLE   PIC 9(05)V99  VALUE 02300.00.
           05  WS-STD-DED-MFJ      PIC 9(05)V99  VALUE 03400.00.
           05  WS-STD-DED-MFS      PIC 9(05)V99  VALUE 01700.00.
           05  WS-STD-DED-HH       PIC 9(05)V99  VALUE 02300.00.
           05  WS-EXEMPTION-VALUE  PIC 9(04)V99  VALUE 1000.00.

      * AUDIT THRESHOLDS
       01  WS-AUDIT-LIMITS.
           05  WS-HIGH-INCOME-LMT  PIC 9(08)V99  VALUE 00100000.00.
           05  WS-HIGH-DED-PCT     PIC V99        VALUE .40.
           05  WS-MAX-AUDIT-CNT    PIC 9(02)      VALUE 03.


      * TAX COPYBOOKS
      *  - INPUT COPYBOOKS
       01  TAX-RETURN-REC                 COPY RETURREC.
       01  TAXPAYER-MASTER-REC            COPY MASTRREC.
      *  - OUTPUT COPYBOKKS
       01  REPORT-REC                    COPY REPORTO.
       01  DISBURSEMENT-REC              COPY DISBRSEO.
       01  AUDIT-FLAG-REC                COPY AUDITO.

      * VSAM IO SETUP COPYBOOKS
       01  VSIO-PARAMETER-VALUES         COPY VSAMIO.
       01  MASTERDB                      COPY VSAMIOFB.
       01  KSDS-RECORD.
           02  KR-KEY                    PIC X(09).
           02  FILLER                    PIC X(71).
      
      **************************************************************
       PROCEDURE DIVISION.                                    
      **************************************************************
           DISPLAY WS-FULL-SEPERATOR.
           DISPLAY WS-MESSAGE.
           PERFORM R9999-PRINT-LOGO.
           DISPLAY WS-FULL-SEPERATOR.

      *    SET DATE - YOU WOULD GET THIS FROM SYSTEM NORMALLY
      *    BUT EASIER HERE TO JUST PRETEND
           MOVE WS-CURRENT-DATE TO WS-RPT-DATE.

      *    OPEN FILES
           PERFORM R1000-OPEN-DATASETS.
           PERFORM R2000-READ-REC-ENTRY.

      *    START MAIN LOOP
           PERFORM R3000-PROCESS-FILE
              UNTIL WS-EOF-RETURNS = 'Y'.

      *    PRINT OUT RESULTS
           PERFORM R4000-FINISH-UP-REPORTS.

      *    CLOSE OUT FILES
           PERFORM R5000-CLOSE-DATASETS.

           STOP RUN.
      
      *  ------
        R1000-OPEN-DATASETS.
      *  ------
           DISPLAY ' -- OPENING DATASETS -- '. 
           OPEN INPUT INPUT-RETURNS-SQ.   
           OPEN OUTPUT OUTPUT-REPORT-SQ.     
           OPEN OUTPUT OUTPUT-DISPERSE-SQ.
           OPEN OUTPUT OUTPUT-AUDIT-SQ.
           DISPLAY ' - OPENED SEQ - '.         

      *    BELOW THIS IS USED TO ACCESS VSAM ON MVS 3.8
           MOVE 'MASTERDB' TO VSIO-DDNAME.
           MOVE VSIO-KSDS TO VSIO-ORGANIZATION.
           MOVE VSIO-DIRECT TO VSIO-ACCESS.
           MOVE VSIO-INPUT-OUTPUT TO VSIO-MODE.
           MOVE +80 TO VSIO-RECORD-LENGTH.
           MOVE +0 TO VSIO-KEY-POSITION.
           MOVE +9 TO VSIO-KEY-LENGTH.
      *    MOVE +10 TO VSIO-KEY-LENGTH.
           MOVE VSIO-OPEN TO VSIO-COMMAND.
           CALL 'VSAMIO' USING VSIO-PARAMETER-BLOCK, MASTERDB,
                               KSDS-RECORD.
           IF NOT VSIO-SUCCESS
               DISPLAY 'VSAMIO ERROR OCCURRED DURING '
                       VSIO-COMMAND
               EXHIBIT NAMED VSIO-RETURN-CODE,
               EXHIBIT NAMED VSIO-VSAM-RETURN-CODE,
                             VSIO-VSAM-FUNCTION-CODE,
                             VSIO-VSAM-FEEDBACK-CODE
               STOP RUN
           ELSE    
               DISPLAY ' - OPENED VSAM - '.        

      *    WRITE REPORT OUTFILE HEADERS
      *       REPORT LOG HEADERS
           MOVE SPACES TO FIL-OUT-REPORT.
           MOVE WS-RPT-HEADER-1 TO FIL-OUT-REPORT.
           WRITE FIL-OUT-REPORT.

           MOVE SPACES TO FIL-OUT-REPORT.
           MOVE WS-RPT-HEADER-2 TO FIL-OUT-REPORT.
           WRITE FIL-OUT-REPORT.

           MOVE SPACES TO FIL-OUT-REPORT.
           MOVE WS-RPT-HEADER-3 TO FIL-OUT-REPORT.
           WRITE FIL-OUT-REPORT.

      *       AUDIT LOG HEADERS
           MOVE SPACES TO FIL-OUT-AUDIT.
           MOVE WS-FULL-SEPERATOR TO FIL-OUT-AUDIT.
           WRITE FIL-OUT-AUDIT.

           MOVE SPACES TO FIL-OUT-AUDIT.
           MOVE ' -- -- TAX AUDIT LOG -- -- ' TO FIL-OUT-AUDIT.
           WRITE FIL-OUT-AUDIT.

           MOVE SPACES TO FIL-OUT-AUDIT.
           MOVE WS-FULL-SEPERATOR TO FIL-OUT-AUDIT.
           WRITE FIL-OUT-AUDIT.

      *       DISPERSE LOG HEADERS
           MOVE SPACES TO FIL-OUT-DISPERSE.
           MOVE WS-FULL-SEPERATOR 
              TO FIL-OUT-DISPERSE.
           WRITE FIL-OUT-DISPERSE.

           MOVE SPACES TO FIL-OUT-DISPERSE.
           MOVE ' -- -- TAX DISPERSEMENT LOG -- -- ' 
              TO FIL-OUT-DISPERSE.
           WRITE FIL-OUT-DISPERSE.

           MOVE SPACES TO FIL-OUT-DISPERSE.
           MOVE WS-FULL-SEPERATOR 
              TO FIL-OUT-DISPERSE.
           WRITE FIL-OUT-DISPERSE.
               


      *  ------
        R2000-READ-REC-ENTRY.
      *  ------
           READ INPUT-RETURNS-SQ INTO TAX-RETURN-REC 
                 AT END MOVE 'Y' TO WS-EOF-RETURNS.
            
           IF WS-EOF-RETURNS = 'N'
               ADD 1 TO WS-RETURNS-READ.


      *  ------
        R3000-PROCESS-FILE.
      *  ------
           MOVE 'N' TO WS-AUDIT-FLAG.
           MOVE 'N' TO WS-MASTER-FOUND.
           MOVE SPACES TO AF-AUDIT-REASON.

           MOVE ZEROES TO WS-AGI.
           MOVE ZEROES TO WS-TAXABLE-INCOME
           MOVE ZEROES TO WS-GROSS-TAX.
           MOVE ZEROES TO WS-NET-TAX.
           MOVE ZEROES TO WS-BALANCE-DUE.
           MOVE ZEROES TO WS-TOTAL-CREDITS.

      *    ONLY EXECUTE IF NOT AT END.
           IF WS-EOF-RETURNS = 'N'

              DISPLAY WS-LINE-SPACE.
              DISPLAY WS-BREAKPT.

      * ATTEMPT TO LOOKUP RECORD FROM SEQ IN DB
              MOVE TAX-RETURN-REC TO KSDS-RECORD.
              DISPLAY 'ATTEMPTING TO READ: ' KR-KEY.
              MOVE VSIO-READ TO VSIO-COMMAND.
              CALL 'VSAMIO' USING VSIO-PARAMETER-BLOCK, MASTERDB,
                                  KSDS-RECORD.
              IF NOT VSIO-SUCCESS
                  IF VSIO-LOGIC-ERROR AND VSIO-RECORD-NOT-FOUND
                     NEXT SENTENCE
                  ELSE
                      DISPLAY 'VSAMIO ERROR OCCURRED DURING '
                              VSIO-COMMAND
                      EXHIBIT NAMED VSIO-RETURN-CODE,
                      EXHIBIT NAMED VSIO-VSAM-RETURN-CODE,
                                    VSIO-VSAM-FUNCTION-CODE,
                                    VSIO-VSAM-FEEDBACK-CODE
                      STOP RUN.
      *
      *     DISPLAY IF FOUND + GO ON WITH CALC
              IF VSIO-LOGIC-ERROR AND VSIO-RECORD-NOT-FOUND
                 DISPLAY '          *** RECORD NOT FOUND'
                 DISPLAY WS-LINE-SPACE
                 ADD 1 TO WS-RETURNS-ERR 
                 STOP RUN
              ELSE
      *          MOVE VSAM DATA INTO COPYBOOK SO WE CAN USE IT
                 MOVE KSDS-RECORD TO TAXPAYER-MASTER-REC

                 DISPLAY '   > RECORD FOUND: ' KSDS-RECORD
                 DISPLAY '   >    FIRSTNAME: ' TM-FIRST-NAME
                 DISPLAY '   >     LASTNAME: ' TM-LAST-NAME
                 DISPLAY '   >          SSN: ' TM-SSN           
                 DISPLAY '   > FILING STATS: ' TM-FILING-STATUS 
                 DISPLAY '   > PRIOR BALANC: ' TM-PRIOR-BALANCE 
                 DISPLAY '   > YTD WITHHELD: ' TM-YTD-WITHHELD  
                 DISPLAY '   >  AUDIT COUNT: ' TM-AUDIT-COUNT   
                 DISPLAY '   > LAST FILE YR: ' TM-LAST-FILING-YR
                 DISPLAY '   >  ADDR LINE 1: ' TM-ADDR-LINE1    
                 DISPLAY '   >         CITY: ' TM-CITY          
                 DISPLAY '   >        STATE: ' TM-STATE         

      *          NOW SETUP AND PROCESS CALCULATIONS
                 PERFORM R3100-TAX-CALCULATIONS
                 PERFORM R3200-AUDIT-CHECK
                 PERFORM R3300-WRITE-RETURN-OUTPUT
                 PERFORM R3400-UPDATE-MASTER-VSAM
                 
                 ADD 1 TO WS-RETURNS-PROC
                 DISPLAY WS-LINE-SPACE   

      *    READ AT END SO DONT GET OFF BY ONE LOOP
                 PERFORM R2000-READ-REC-ENTRY.           
           
      * 

      *  ------
        R3100-TAX-CALCULATIONS.
      *  ------
      * CALC AVERAGE INCOME
           DISPLAY '   > CALC AVERAGE INCOME'.
           COMPUTE WS-AGI = TR-WAGES + TR-INTEREST-INCOME +
                 TR-DIVIDENDS.


      * CALC DEDUCTIONS
           DISPLAY '   > CALC DEDUCTIONS'.
           MOVE ZEROES TO WS-STD-DEDUCTION
      *    MAYBE FOR DEDUCTIONS, DO IF TR DEDUCT GREATEER FIRST, THEN
      *    DO THE REST AFTER, SAVE PROCESS CYCLES THAT WAY

      *    FIND GREATER DEDUCTION AND USE THAT
           IF TR-DEDUCTIONS > WS-STD-DEDUCTION
              MOVE TR-DEDUCTIONS TO WS-STD-DEDUCTION
              DISPLAY '      TR-DEDUCTIONS GREATER'
           ELSE
      *       SET SINGLE AS DEFAULT
              MOVE WS-STD-DED-SINGLE TO WS-STD-DEDUCTION

              IF TM-MARRIED-JOINT
                 MOVE ZEROES TO WS-STD-DEDUCTION
                 MOVE WS-STD-DED-MFJ TO WS-STD-DEDUCTION

              IF TM-MARRIED-SEP
                 MOVE ZEROES TO WS-STD-DEDUCTION
                 MOVE WS-STD-DED-MFS TO WS-STD-DEDUCTION

              IF TM-HEAD-HOUSEHOLD
                 MOVE ZEROES TO WS-STD-DEDUCTION
                 MOVE WS-STD-DED-HH TO WS-STD-DEDUCTION
              
              DISPLAY '      WS-STD-DEDUCTION GREATER'.
      *    I ANTICIPATE THESE NESTED IF AND ELSE STATEMENTS BE
      *    TROUBLESOME TO SAY THE LEAST, ADD PRINT OUTS


      * CALC TAXABLE INCOME
           DISPLAY '   > CALC TAXABLE INCOME'.
      *    EXECEPTION CALC
           COMPUTE WS-EXEMPTION-AMT =
              TR-EXEMPTIONS * WS-EXEMPTION-VALUE.

           COMPUTE WS-TAXABLE-INCOME =
               WS-AGI - WS-STD-DEDUCTION - WS-EXEMPTION-AMT.

      * CALC GROSS TAX -- TAX BRACKETS BELOW
           DISPLAY '   > CALC GROSS TAX'.
           IF WS-TAXABLE-INCOME < 1
               MOVE ZEROS TO WS-TAXABLE-INCOME.
               MOVE ZEROS TO WS-GROSS-TAX.
      *        IF THEIR INCOME IS LESS THAN 1, THEY HAVE BIGGER
      *        PROBLEMS THAN THE IRS
           
           IF WS-TAXABLE-INCOME < 2100.00 AND WS-TAXABLE-INCOME > 0
                 COMPUTE WS-GROSS-TAX = WS-TAXABLE-INCOME * .14.

           IF WS-TAXABLE-INCOME < 4200.00 AND 
              WS-TAXABLE-INCOME > 2099.99
                 COMPUTE WS-GROSS-TAX = 294.00 +
                    (WS-TAXABLE-INCOME - 2100.00) * .15.

           IF WS-TAXABLE-INCOME < 6500.00 AND 
              WS-TAXABLE-INCOME > 4199.99
                 COMPUTE WS-GROSS-TAX =
                  609.00 + (WS-TAXABLE-INCOME - 4200.00) * .16.

           IF WS-TAXABLE-INCOME < 8200.00 AND
              WS-TAXABLE-INCOME > 6499.99
                 COMPUTE WS-GROSS-TAX =
                  977.00 + (WS-TAXABLE-INCOME - 6500.00) * .17.

           IF WS-TAXABLE-INCOME < 10800.00 AND
              WS-TAXABLE-INCOME > 8199.99
                 COMPUTE WS-GROSS-TAX =
                  1266.00 + (WS-TAXABLE-INCOME - 8200.00) * .19.

           IF WS-TAXABLE-INCOME > 10799.99
                 COMPUTE WS-GROSS-TAX = 1760.00 +
                  (WS-TAXABLE-INCOME - 10800.00) * .22.
           

      * CALC CREDITS TO APPLY
           DISPLAY '   > CALC CREDITS TO APPLY'.
      *    LIKE CHILD TAX CREDITS
           MOVE TR-CHILD-TAX-CRED TO WS-TOTAL-CREDITS.

           COMPUTE WS-NET-TAX = WS-GROSS-TAX - WS-TOTAL-CREDITS.

           IF WS-NET-TAX < 0
               MOVE ZERO TO WS-NET-TAX.


      * CALC BALANCE
           DISPLAY '   > CALC BALANCE'.
      *    POSITIVE = TAX OWED TO IRS
      *    NEGATIVE = REFUND DUE TO TAXPAYER
           COMPUTE WS-BALANCE-DUE = WS-NET-TAX - TR-WITHHELD.

           IF WS-BALANCE-DUE < 0
               ADD 1 TO WS-REFUNDS-ISSUED
               COMPUTE WS-WORK-AMT = WS-BALANCE-DUE * -1
               ADD WS-WORK-AMT TO WS-TOTAL-REFUNDS
           ELSE
               ADD 1 TO WS-TAXES-OWED
               ADD WS-BALANCE-DUE TO WS-TOTAL-COLLECTED.

           DISPLAY '   > COMPLETED R3100-TAX-CALCULATIONS!'.


      *  ------
        R3200-AUDIT-CHECK.
      *  ------
           DISPLAY '   ~ R3200-AUDIT-CHECK'.
      * CHECK TO SEE IF WE NEED TO HAVE AN AUDIT ON THE PERSON
           MOVE SPACES TO AF-AUDIT-REASON.
           
      *    OVER $100,000 - YES
           IF WS-AGI > WS-HIGH-INCOME-LMT
               MOVE 'Y' TO WS-AUDIT-FLAG
               MOVE 'HIGH INCOME OVER $100K'
                   TO AF-AUDIT-REASON.

      *    EXCESSIVE DEDUCTIONS -- OVER 40% OF AGI - YES
           IF WS-AGI > 0
               COMPUTE WS-DED-RATIO = TR-DEDUCTIONS / WS-AGI
               IF WS-DED-RATIO > WS-HIGH-DED-PCT AND 
                 WS-AUDIT-FLAG = 'N'
                   MOVE 'Y' TO WS-AUDIT-FLAG
                   MOVE 'EXCESSIVE DEDUCTION RATIO'
                       TO AF-AUDIT-REASON.

      *    3 OR MORE PRIOR AUDITS - YES
           IF TM-AUDIT-COUNT > WS-MAX-AUDIT-CNT AND 
              WS-AUDIT-FLAG = 'N'
               MOVE 'Y' TO WS-AUDIT-FLAG
               MOVE 'PRIOR AUDIT HISTORY - 3 OR MORE'
                   TO AF-AUDIT-REASON.

      *    PRIOR YEAR BALANCE - YES
           IF TM-PRIOR-BALANCE > 0 AND 
              WS-AUDIT-FLAG = 'N'
               MOVE 'Y' TO WS-AUDIT-FLAG
               MOVE 'OUTSTANDING PRIOR YEAR BALANCE'
                   TO AF-AUDIT-REASON.

           IF NEEDS-AUDIT
               ADD 1 TO WS-AUDITS-FLAGGED.


      *  ------
        R3300-WRITE-RETURN-OUTPUT.
      *  ------
           DISPLAY '   ^ R3300-WRITE-RETURN-OUTPUT'.
      *    WRITE REFUND DISBURSEMENT RECORD IF DUE
           IF WS-BALANCE-DUE < 0
               MOVE TR-SSN TO DB-SSN
               MOVE TM-LAST-NAME TO DB-LAST-NAME
               MOVE TM-FIRST-NAME TO DB-FIRST-NAME
               COMPUTE DB-REFUND-AMOUNT = WS-BALANCE-DUE * -1
               MOVE TR-TAX-YEAR TO DB-TAX-YEAR
               MOVE TM-ADDR-LINE1 TO DB-ADDR-LINE1
               MOVE TM-CITY TO DB-CITY

               MOVE SPACES TO FIL-OUT-DISPERSE.
               MOVE DISBURSEMENT-REC TO FIL-OUT-DISPERSE.
               WRITE FIL-OUT-DISPERSE.

      *    WRITE AUDIT FLAG RECORD IF FLAGGED
           IF NEEDS-AUDIT
               MOVE TR-SSN TO AF-SSN
               MOVE TM-LAST-NAME TO AF-LAST-NAME
               MOVE TR-TAX-YEAR TO AF-TAX-YEAR
               MOVE WS-AGI TO AF-GROSS-INCOME
               MOVE WS-NET-TAX TO AF-CALC-TAX

               MOVE SPACES TO FIL-OUT-AUDIT.
               MOVE AUDIT-FLAG-REC TO FIL-OUT-AUDIT.
               WRITE FIL-OUT-AUDIT.

      *    WRITE DETAIL LINE TO PROCESSING REPORT
           MOVE SPACES TO WS-RPT-DETAIL.
           MOVE TR-SSN TO WS-RPT-SSN.
           MOVE TM-LAST-NAME TO WS-RPT-NAME.
           MOVE TR-TAX-YEAR TO WS-RPT-YEAR.
           MOVE WS-AGI TO WS-RPT-INCOME.
           MOVE WS-NET-TAX TO WS-RPT-TAX.
           MOVE TR-WITHHELD TO WS-RPT-WITHHELD.
           MOVE WS-BALANCE-DUE TO WS-RPT-BALANCE.

      *    DONT THINK THE COMPILER IS GOING TO THESE NESTED STATEMENTS
           IF WS-BALANCE-DUE < 0
               IF NEEDS-AUDIT
                   MOVE 'REFUND/AUD' TO WS-RPT-DISP-STAT
               ELSE
                   MOVE 'REFUND    ' TO WS-RPT-DISP-STAT.

           IF WS-BALANCE-DUE > -1
               IF NEEDS-AUDIT
                   MOVE 'OWED/AUDIT' TO WS-RPT-DISP-STAT
               ELSE
                   MOVE 'TAX OWED  ' TO WS-RPT-DISP-STAT.

           MOVE SPACES TO FIL-OUT-REPORT.
           MOVE WS-RPT-DETAIL TO FIL-OUT-REPORT.
           WRITE FIL-OUT-REPORT.


      *  ------
        R3400-UPDATE-MASTER-VSAM.
      *  ------
           DISPLAY '   < R3400-UPDATE-MASTER-VSAM'.

           MOVE TR-TAX-YEAR  TO TM-LAST-FILING-YR.
           MOVE TR-WITHHELD  TO TM-YTD-WITHHELD.

           IF NEEDS-AUDIT
               ADD 1 TO TM-AUDIT-COUNT.

      *    UPDATE VSAM WITH VALUES FROM SEQ FILE USING VSAMIO COMMANDS
      *    MOVE RI-IMAGE TO KSDS-RECORD.
           MOVE VSIO-REWRITE TO VSIO-COMMAND.
           CALL 'VSAMIO' USING VSIO-PARAMETER-BLOCK, MASTERDB,
                               KSDS-RECORD.
           IF VSIO-SUCCESS
               DISPLAY KSDS-RECORD
               DISPLAY '               RECORD AFTER CHANGE'
           ELSE
               DISPLAY 'VSAMIO ERROR OCCURRED DURING '
                       VSIO-COMMAND
               EXHIBIT NAMED VSIO-RETURN-CODE,
               EXHIBIT NAMED VSIO-VSAM-RETURN-CODE,
                             VSIO-VSAM-FUNCTION-CODE,
                             VSIO-VSAM-FEEDBACK-CODE
               STOP RUN.

      *  ------
        R4000-FINISH-UP-REPORTS.
      *  ------
           DISPLAY 'FINALLY: R4000-FINISH-UP-REPORTS.'
           DISPLAY WS-LINE-SPACE.

           MOVE SPACES TO FIL-OUT-REPORT.
           MOVE WS-RPT-HEADER-3 TO FIL-OUT-REPORT.
           WRITE FIL-OUT-REPORT.

           MOVE SPACES TO FIL-OUT-REPORT.
           MOVE WS-TOTAL-REFUNDS    TO WS-TOT-REFUNDS.
           MOVE WS-TOTAL-COLLECTED  TO WS-TOT-COLLECT.
           MOVE WS-RPT-TOTALS TO FIL-OUT-REPORT.
           WRITE FIL-OUT-REPORT.

           MOVE SPACES TO FIL-OUT-REPORT.
           MOVE WS-RETURNS-PROC     TO WS-CNT-PROC.
           MOVE WS-REFUNDS-ISSUED   TO WS-CNT-REFUND.
           MOVE WS-AUDITS-FLAGGED   TO WS-CNT-AUDIT.
           MOVE WS-RETURNS-ERR      TO WS-CNT-ERR.
           MOVE WS-RPT-COUNTS TO FIL-OUT-REPORT.
           WRITE FIL-OUT-REPORT.

           DISPLAY WS-FULL-SEPERATOR.
           DISPLAY 'TAXPROC: PROCESSING COMPLETE.'.
           DISPLAY 'TAXPROC: RETURNS READ    = ' WS-RETURNS-READ.
           DISPLAY 'TAXPROC: RETURNS PROC    = ' WS-RETURNS-PROC.
           DISPLAY 'TAXPROC: REFUNDS ISSUED  = ' WS-REFUNDS-ISSUED.
           DISPLAY 'TAXPROC: AUDITS FLAGGED  = ' WS-AUDITS-FLAGGED.
           DISPLAY 'TAXPROC: ERRORS/REJECTS  = ' WS-RETURNS-ERR.
           DISPLAY WS-FULL-SEPERATOR.


      * ------------------
        R5000-CLOSE-DATASETS.
      * ------------------
           CLOSE INPUT-RETURNS-SQ.
           CLOSE OUTPUT-REPORT-SQ.
           CLOSE OUTPUT-DISPERSE-SQ.
           CLOSE OUTPUT-AUDIT-SQ.
           
           MOVE VSIO-CLOSE TO VSIO-COMMAND.
           CALL 'VSAMIO' USING VSIO-PARAMETER-BLOCK, MASTERDB,
                               KSDS-RECORD.
                               
           DISPLAY ' -- CLOSED DATASETS -- '.

      * ------------------
       R9999-PRINT-LOGO.
      * ------------------
        DISPLAY 
        ' __  __   __                                                 '. 
        DISPLAY 
        '|  |/  `.*  `..-.          .-                                '. 
        DISPLAY 
        '|   .-.  .-   *\ \        / /  .|                            '. 
        DISPLAY 
        '|  |  |  | |  | \ \      / / .* |_     __     ____     ____  '. 
        DISPLAY 
        '|  |  |  | |  |  \ \    / /.*     | .:--.*.  `.   \  .*   /  '. 
        DISPLAY 
        '|  |  |  | |  |   \ \  / /*--.  .-*/ |   \ |   `.  `*   .*   '. 
        DISPLAY 
        '|  |  |  | |  |    \ `  /    |  |  `" __ | |     *.    .     '. 
        DISPLAY 
        '|__|  |__| |__|     \  /     |  |   .*.**| |     .*     .    '. 
        DISPLAY 
        '                    / /      |  *.*/ /   | |_  .*  .*`.  `.  '. 
        DISPLAY 
        '                |`-* /       |   / \ \._,\ */.*   /    `   `.'. 
        DISPLAY 
        '                 *..*        `*-*   `--*  `"*----*      *----'.
./ ENDUP 
><       
/*       
//*********************************************************************
//* CREATE A PDS WITH COPYBOOKS                                       *
//*********************************************************************
//*                                                                    
//STEP02 EXEC PGM=IEBUPDTE,REGION=1024K,PARM=NEW                     
//SYSPRINT DD  SYSOUT=*                                                
//*                                                                    
//SYSUT2   DD  DSN=PRJ4.DEV.COPYBOOK,DISP=(,CATLG,DELETE),            
//             UNIT=TSO,SPACE=(TRK,(15,,2)),                           
//             DCB=(RECFM=FB,LRECL=80,BLKSIZE=800)                     
//SYSPRINT DD  SYSOUT=*                                                
//SYSIN    DD  DATA,DLM='><'                                           
./ ADD NAME=AUDITO,LIST=ALL                                           
      ****************************************************************
      * AUDIT FILE                                                   *
      * FILE: AUDITO.CPY                                             *
      * RECORD LENGTH: 80 BYTES                                      *
      ****************************************************************
       01  COPY-AUDIT-REC.
           05  AF-SSN              PIC X(09).
           05  AF-LAST-NAME        PIC X(15).
           05  AF-TAX-YEAR         PIC 9(04).
           05  AF-AUDIT-REASON     PIC X(30).
           05  AF-GROSS-INCOME     PIC 9(08)V99.
           05  AF-CALC-TAX         PIC 9(07)V99.
           05  FILLER              PIC X(03).      
./ ADD NAME=DISBRSEO,LIST=ALL     
      ****************************************************************
      * DISBURSEMENT FILE                                            *
      * FILE: DISBRSEO.CPY                                           *
      * RECORD LENGTH: 80 BYTES                                      *
      ****************************************************************
       01  COPY-DISBURSEMENT-REC.
           05  DB-SSN              PIC X(09).
           05  DB-LAST-NAME        PIC X(15).
           05  DB-FIRST-NAME       PIC X(10).
           05  DB-REFUND-AMOUNT    PIC 9(07)V99.
           05  DB-TAX-YEAR         PIC 9(04).
           05  DB-ADDR-LINE1       PIC X(20).
           05  DB-CITY             PIC X(12).
           05  FILLER              PIC X(01).     
./ ADD NAME=MASTRREC,LIST=ALL     
      ****************************************************************
      * TAXPAYER MASTER FILE - SSN NAME FILING STATUS, ETC           *
      * FILE: MASTRREC.CPY                                           *
      * RECORD LENGTH: 80 BYTES                                      *
      ****************************************************************
       01  COPY-TAX-MSTR-REC.
           05  TM-SSN              PIC X(09).
           05  TM-LAST-NAME        PIC X(09).  
           05  TM-FIRST-NAME       PIC X(09).  
           05  TM-FILING-STATUS    PIC X(01).
               88  TM-SINGLE           VALUE 'S'.
               88  TM-MARRIED-JOINT    VALUE 'M'.
               88  TM-MARRIED-SEP      VALUE 'P'.
               88  TM-HEAD-HOUSEHOLD   VALUE 'H'.
           05  TM-PRIOR-BALANCE    PIC S9(07)V99.  
           05  TM-YTD-WITHHELD     PIC S9(07)V99.  
           05  TM-AUDIT-COUNT      PIC 9(02).      
           05  TM-LAST-FILING-YR   PIC 9(04).
           05  TM-ADDR-LINE1       PIC X(14).
           05  TM-CITY             PIC X(12).
           05  TM-STATE            PIC X(02).
           05  FILLER              PIC X(15).
./ ADD NAME=REPORTO,LIST=ALL     
      ****************************************************************
      * REPORT FILE                                                  *
      * FILE: REPORTO.CPY                                            *
      * RECORD LENGTH: 132 BYTES                                     *
      ****************************************************************
       01  COPY-REPORT-REC.
           02  WS-RPT-HEADER-1.
              05  FILLER     PIC X(40)
                  VALUE '   FEDERAL TAX FILING PROCESSOR - BATCH'.
              05  FILLER     PIC X(40)
                  VALUE ' PROCESSING REPORT                      '.
              05  FILLER     PIC X(52) VALUE SPACES.
           02  WS-RPT-HEADER-2.
              05  FILLER     PIC X(20) VALUE '   DATE PROCESSED: '.
              05  WS-RPT-DATE PIC X(10).
              05  FILLER     PIC X(102) VALUE SPACES.
           02  WS-RPT-HEADER-3.
              05  FILLER     PIC X(132) VALUE ALL '-'.
           02  WS-RPT-DETAIL.
              05  FILLER          PIC X(03) VALUE SPACES.
              05  WS-RPT-SSN      PIC X(11).
              05  WS-RPT-NAME     PIC X(26).
              05  WS-RPT-YEAR     PIC 9(04).
              05  FILLER          PIC X(02) VALUE SPACES.
              05  WS-RPT-INCOME   PIC ZZZ,ZZZ,ZZ9.99.
              05  FILLER          PIC X(02) VALUE SPACES.
              05  WS-RPT-TAX      PIC ZZZZZ,ZZ9.99.
              05  FILLER          PIC X(02) VALUE SPACES.
              05  WS-RPT-WITHHELD PIC ZZZZZ,ZZ9.99.
              05  FILLER          PIC X(02) VALUE SPACES.
              05  WS-RPT-BALANCE  PIC S9(07)V99.
              05  FILLER          PIC X(02) VALUE SPACES.
              05  WS-RPT-DISP-STAT PIC X(10).
              05  FILLER          PIC X(14) VALUE SPACES.
           02  WS-RPT-TOTALS.
              05  FILLER          PIC X(03) VALUE SPACES.
              05  FILLER          PIC X(20)
                     VALUE 'TOTAL REFUNDS ISSUED'.
              05  WS-TOT-REFUNDS  PIC $$$,$$$,$$9.99.
              05  FILLER          PIC X(03) VALUE SPACES.
              05  FILLER          PIC X(20)
                     VALUE 'TOTAL TAX COLLECTED '.
              05  WS-TOT-COLLECT  PIC $$$,$$$,$$9.99.
              05  FILLER          PIC X(60) VALUE SPACES.
           02  WS-RPT-COUNTS.
              05  FILLER          PIC X(03) VALUE SPACES.
              05  FILLER          PIC X(20) VALUE 'RETURNS PROCESSED: '.
              05  WS-CNT-PROC     PIC ZZZ,ZZ9.
              05  FILLER          PIC X(03) VALUE SPACES.
              05  FILLER          PIC X(20) VALUE 'REFUNDS ISSUED:    '.
              05  WS-CNT-REFUND   PIC ZZZ,ZZ9.
              05  FILLER          PIC X(03) VALUE SPACES.
              05  FILLER          PIC X(20) VALUE 'AUDITS FLAGGED:    '.
              05  WS-CNT-AUDIT    PIC ZZZ,ZZ9.
              05  FILLER          PIC X(03) VALUE SPACES.
              05  FILLER          PIC X(20) VALUE 'ERRORS/REJECTS:    '.
              05  WS-CNT-ERR      PIC ZZZ,ZZ9.
              05  FILLER          PIC X(05) VALUE SPACES.
./ ADD NAME=RETURREC,LIST=ALL  
      ****************************************************************
      * TAXPAYER TAX RETURN FILE - FLAT SEQUENTIAL FILE W YEARLY INFO*
      * FILE: RETURREC.CPY                                           *
      * RECORD LENGTH: 80 BYTES                                      *
      * MAKE SURE THAT THESE ARE WORKING AND ALIGNED CORRECTLY***    *
      ****************************************************************
       01  COPY-RETURN-REC.
           05  TR-SSN              PIC X(09).
           05  TR-TAX-YEAR         PIC 9(04).
           05  TR-GROSS-INCOME     PIC S9(08)V99.
           05  TR-WAGES            PIC S9(08)V99.
           05  TR-INTEREST-INCOME  PIC S9(06)V99.
           05  TR-DIVIDENDS        PIC S9(06)V99.
           05  TR-DEDUCTIONS       PIC S9(07)V99.
           05  TR-EXEMPTIONS       PIC 9(02).    
           05  TR-WITHHELD         PIC S9(07)V99.
           05  TR-CHILD-TAX-CRED   PIC S9(05)V99.
           05  TR-FILING-STATUS    PIC X(01).
           05  FILLER              PIC X(03).    
./ ENDUP    
><          
/*          
//*********************************************************************
//* CREATE A PDS WITH JCL                                             *
//*********************************************************************
//*                                                                    
//STEP03 EXEC PGM=IEBUPDTE,REGION=1024K,PARM=NEW                     
//SYSPRINT DD  SYSOUT=*                                                
//*                                                                    
//SYSUT2   DD  DSN=PRJ4.DEV.JCL,DISP=(,CATLG,DELETE),            
//             UNIT=TSO,SPACE=(TRK,(15,,2)),                           
//             DCB=(RECFM=FB,LRECL=80,BLKSIZE=800)                     
//SYSPRINT DD  SYSOUT=*                                                
//SYSIN    DD  DATA,DLM='><'                                           
./ ADD NAME=S2FILES,LIST=ALL  
//PRJ402A JOB (PT2SEQ),'SETUP SEQ',CLASS=A,
//            MSGCLASS=X,MSGLEVEL=(1,1),NOTIFY=&SYSUID
//* THIS PROGRAM WAS ORIGINALY MADE BY JAY MOSELEY                      
//********************************************************************* 
//* CREATE FLAT FILES, VSAM, LOAD FLAT FILES INTO VSAM USING KSDSLOAD
//* DELETE EXISTING PRJ4.DEV.MASTER.DATA (IF ANY)                     * 
//********************************************************************* 
//IDCAMS  EXEC PGM=IDCAMS,REGION=1024K                                  
//SYSPRINT DD  SYSOUT=*                                                 
//SYSIN    DD  *                                                        
    DELETE PRJ4.DEV.MASTER.DATA NONVSAM                                 
    DELETE PRJ4.DEV.RETURN.DATA NONVSAM     
    SET MAXCC=0
/*                                                                      
//********************************************************************* 
//* CREATE A SEQUENTIAL DATASET FROM INSTREAM TEST IMAGES - MASTER FILE 
//********************************************************************* 
//STEP01 EXEC PGM=IEBGENER,REGION=128K                                
//SYSIN    DD  DUMMY                                                    
//SYSPRINT DD  SYSOUT=*                                                 
//*                                                                     
//SYSUT2   DD  DSN=PRJ4.DEV.MASTER.DATA,DISP=(,CATLG,DELETE),           
//             UNIT=TSO,SPACE=(TRK,(15),RLSE),          
//             DCB=(RECFM=FB,LRECL=80,BLKSIZE=800)      
//* NOTE: IF YOU TRY TO PASTE THE DATA IN, IT WILL GO INTO TWO LINES
//* IF YOUR SCREEN IS TOO SMALL, TRY PASTING IN CHUNKS 
//SYSUT1   DD  *                                                  
111223333ANDERSON JAMES    S000000000000250000001977100 ELM STR   ALBANY      NY
222334444BAKER    LINDA    S000000000000120000001977204 OAK AVE   SAN DIEGO   CA
333445555CALDWELL ROBERT   S000000000002800000011977500 PARK BLVD SACRAMENTO  CA
444556666DAVIS    PATRICIA S00000000000030000000197788 PINE ROAD  CHARLESTON  SC
555667777EDWARDS  THOMAS   S00000000000040000004197712 CEDAR LANE DAYTON      OH
666778888FOSTER   MARY     S000150000000280000001977300 MAPLE DR  SMITHFIELD  RI
777889999GARCIA   CARLOS   M00000000000060000000197742 BIRCH STR  MERIMACK    NH
888990000HARRIS   SUSAN    H0000000000002200000019777 WILLOW WAY  WESTLAKE    TX
999001111INGRAM   DAVID    M00000000000035000000197755 ASH COURT  BOSTON      MA
/*                                                                      
//SYSOUT   DD  SYSOUT=*                                   
//********************************************************************* 
//* CREATE A SEQUENTIAL DATASET - RETURNS FILE (NOT FOR VSAM)
//********************************************************************* 
//STEP02 EXEC PGM=IEBGENER,REGION=128K                                
//SYSIN    DD  DUMMY                                                    
//SYSPRINT DD  SYSOUT=*                                                 
//*                                                                     
//SYSUT2   DD  DSN=PRJ4.DEV.RETURN.DATA,DISP=(,CATLG,DELETE),           
//             UNIT=TSO,SPACE=(TRK,(15),RLSE),          
//             DCB=(RECFM=FB,LRECL=80,BLKSIZE=800)      
//* NOTE: IF YOU TRY TO PASTE THE DATA IN, IT WILL GO INTO TWO LINES
//* IF YOUR SCREEN IS TOO SMALL, TRY PASTING IN CHUNKS 
//SYSUT1   DD  *  
1112233331978000180000000018000000000000000000000000000000010002500000000000S   
2223344441978000120000000012000000000000000000000000000000010001200000000000S   
3334455551978001250000000115000000050000000500000000000000010028000000000000S   
4445566661978000200000000020000000000000000000000001000000010003000000000000S   
5556677771978000350000000035000000000000000000000000000000010004000000000000S   
6667788881978000220000000022000000000000000000000000000000010002800000000000S   
7778899991978000280000000026000000010000000100000000000000020006000000000000M   
8889900001978000190000000019000000000000000000000000000000020002200000000000H   
9990011111978000320000000030000000010000000100000000000000040003500000040000M   
/*                       
//SYSOUT   DD  SYSOUT=*                         
//********************************************************************* 
//* CREATE VSAM KEY DATA CLUSTER DB
//********************************************************************* 
//STEP03 EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
  DELETE HERC01.PRJ4.MASTERDB
  SET MAXCC = 0
  DEFINE CLUSTER ( -
           NAME(HERC01.PRJ4.MASTERDB) -
           VOLUME(TSO002) -
           TRACKS(1,1) -
           CONTROLINTERVALSIZE(4096) -
           FREESPACE(10,20) -
           RECORDSIZE(80,80) -
           KEYS(10,0) -
           UNIQUE ) -
         DATA ( -
           NAME(HERC01.PRJ4.MASTERDB.DATA) ) -
         INDEX ( -
           NAME(HERC01.PRJ4.MASTERDB.INDEX) -
           CONTROLINTERVALSIZE(1024) ) -
         CATALOG(SYS1.UCAT.TSO)
/*
//********************************************************************* 
//* COMPILE LOAD PROG + THEN FILL WITH RECORDS
//********************************************************************* 
//STEP04  EXEC COBUCLG,
//            PARM.COB='LOAD,SIZE=2048K,BUF=1024K,LIB'
//COB.SYSLIB DD DSN=SYS2.VSAMIO.SOURCE,DISP=SHR
//COB.SYSPUNCH DD SYSOUT=B
//COB.SYSIN DD DSN=SYS2.VSAMIO.SOURCE(KSDSLOAD),DISP=SHR
//LKED.SYSLIN DD
//         DD DSN=SYS2.VSAMIO.OBJECT(VSAMIO),DISP=SHR
//GO.IMAGES DD DSN=PRJ4.DEV.MASTER.DATA,DISP=SHR
//GO.KSDSF01 DD DSN=HERC01.PRJ4.MASTERDB,DISP=OLD
//GO.SYSOUT DD SYSOUT=*
./ ADD NAME=S3RUN,LIST=ALL
//* LOAD CLUSTER WITH RECORDS
//PRJ403A JOB (PRJ403A),
//            'COMPILE RUN',
//            CLASS=A,MSGCLASS=X,MSGLEVEL=(1,1),
//            NOTIFY=&SYSUID
//*********************************************************************
//* DELETE EXISTING OUTPUT FILES (IF ANY)                             *
//*********************************************************************
//CLEANUP1 EXEC PGM=IEFBR14
//LICOUT   DD DSN=PRJ4.DEV.OUTPUT.REPORT,DISP=(MOD,DELETE,DELETE),
//         SPACE=(TRK,(0,0)),UNIT=SYSDA
//*
//CLEANUP2 EXEC PGM=IEFBR14
//LICOUT   DD DSN=PRJ4.DEV.OUTPUT.DISPERSE,DISP=(MOD,DELETE,DELETE),
//         SPACE=(TRK,(0,0)),UNIT=SYSDA
//*
//CLEANUP3 EXEC PGM=IEFBR14
//LICOUT   DD DSN=PRJ4.DEV.OUTPUT.AUDIT,DISP=(MOD,DELETE,DELETE),
//         SPACE=(TRK,(0,0)),UNIT=SYSDA
//*********************************************************************
//* COMPILE PROG + RUN                                                *
//*********************************************************************
//COMPILE4  EXEC COBUCLG,
//            PARM.COB='LOAD,SIZE=2048K,BUF=1024K,LIB'
//COB.SYSLIB   DD DSN=PRJ4.DEV.COPYBOOK,DISP=SHR
//             DD DSN=SYS2.VSAMIO.SOURCE,DISP=SHR
//COB.SYSPUNCH DD SYSOUT=B
//COB.SYSIN    DD DSN=PRJ4.DEV.BCOB(MYTAX),DISP=SHR
//LKED.SYSLIN  DD
//             DD DSN=SYS2.VSAMIO.OBJECT(VSAMIO),DISP=SHR
//GO.RETURNS   DD DSN=PRJ4.DEV.RETURN.DATA,DISP=SHR
//GO.MASTERDB  DD DSN=HERC01.PRJ4.MASTERDB,DISP=OLD
//*
//* REPORT FILE - 132 BYTE RECORDS (FB)
//GO.REPORT    DD DSN=PRJ4.DEV.OUTPUT.REPORT,
//             DISP=(NEW,CATLG,DELETE),
//             UNIT=SYSDA,
//             SPACE=(TRK,(5,5),RLSE),
//             DCB=(RECFM=FB,LRECL=132,BLKSIZE=1320)
//*
//* DISPERSEMENT FILE - 80 BYTE RECORDS (FB)
//GO.DISPERSE  DD DSN=PRJ4.DEV.OUTPUT.DISPERSE,
//             DISP=(NEW,CATLG,DELETE),
//             UNIT=SYSDA,
//             SPACE=(TRK,(5,5),RLSE),
//             DCB=(RECFM=FB,LRECL=80,BLKSIZE=800)
//*
//* AUDIT FILE - 80 BYTE RECORDS (FB)
//GO.AUDIT     DD DSN=PRJ4.DEV.OUTPUT.AUDIT,
//             DISP=(NEW,CATLG,DELETE),
//             UNIT=SYSDA,
//             SPACE=(TRK,(5,5),RLSE),
//             DCB=(RECFM=FB,LRECL=80,BLKSIZE=800)
//*
//GO.SYSOUT    DD SYSOUT=*
//
//* MASTERDB IS THE VARIABLE WE USE IN THE COBOL PROG ASSIGNED TO THE
//* NAME OF THE VSAM KSDS DATABASE. IF WE WANT TO USE MULTIPLE VSAM,
//* WE WILL NEED TO SETUP MULTIPLE VARIABLES FOR EACH DB AND ALSO
//* MULTIPLE VSAMIO COPYBOOKS/COMMANDBOOKS FOR EACH
//
./ ENDUP 
><       
/*       
