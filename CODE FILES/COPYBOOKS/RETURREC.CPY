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