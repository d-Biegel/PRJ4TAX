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
