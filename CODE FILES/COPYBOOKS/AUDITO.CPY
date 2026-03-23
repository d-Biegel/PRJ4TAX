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
