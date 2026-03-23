      ****************************************************************
      * DISBURSEMENT FILE                                            *
      * FILE: DISPRSEO.CPY                                           *
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
