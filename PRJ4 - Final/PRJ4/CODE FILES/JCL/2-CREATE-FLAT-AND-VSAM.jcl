//* CREATE FLAT FILES, VSAM, LOAD FLAT FILES INTO VSAM USING KSDSLOAD
//PRJ402A JOB (PT2SEQ),'SETUP SEQ',CLASS=A,
//            MSGCLASS=X,MSGLEVEL=(1,1),NOTIFY=&SYSUID
//* THIS PROGRAM WAS ORIGINALY MADE BY JAY MOSELEY                      
//********************************************************************* 
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
//
