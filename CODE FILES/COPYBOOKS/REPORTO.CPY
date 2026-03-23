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
