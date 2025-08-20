      *----------------------------------------------------------------*
      *  IEBGENER – Minimal IEBGENER-like copy utility in COBOL        *
      *  - Copies SYSUT1 -> SYSUT2 (QSAM), supports FB or VB datasets  *
      *  - Emits a one-line summary to SYSPRINT                        *
      *----------------------------------------------------------------*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. IEBGENER.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INFILE    ASSIGN TO SYSUT1
                            ORGANIZATION IS SEQUENTIAL
                            FILE STATUS IS IN-STAT.
           SELECT OUTFILE   ASSIGN TO SYSUT2
                            ORGANIZATION IS SEQUENTIAL
                            FILE STATUS IS OUT-STAT.
           SELECT PRTFILE   ASSIGN TO SYSPRINT
                            ORGANIZATION IS SEQUENTIAL
                            FILE STATUS IS PRT-STAT.

       DATA DIVISION.
       FILE SECTION.
       FD  INFILE
           RECORDING MODE IS V
           RECORD VARYING  FROM 1 TO 32756 DEPENDING ON IN-LEN.
       01  INREC    PIC X(32756).

       FD  OUTFILE
           RECORDING MODE IS V
           RECORD VARYING  FROM 1 TO 32756 DEPENDING ON OUT-LEN.
       01  OUTREC   PIC X(32756).

       FD  PRTFILE
           RECORDING MODE IS F
           RECORD CONTAINS 133 CHARACTERS.
       01  PRTLINE  PIC X(133).

       WORKING-STORAGE SECTION.
       01  IN-STAT     PIC XX  VALUE SPACES.
       01  OUT-STAT    PIC XX  VALUE SPACES.
       01  PRT-STAT    PIC XX  VALUE SPACES.

       01  IN-LEN      PIC 9(5) COMP VALUE 0.
       01  OUT-LEN     PIC 9(5) COMP VALUE 0.

       01  WS-COUNT    PIC 9(18) COMP-3 VALUE 0.
       01  WS-EOF      PIC X VALUE 'N'.

       01  WS-DATE-TIME.
           05  WS-CURR-DT PIC X(10).
           05  WS-CURR-TM PIC X(8).

       01  CURR-DATE-TIME.
           05  CDT-YEAR    PIC 9(4).
           05  CDT-MONTH   PIC 9(2).
           05  CDT-DAY     PIC 9(2).
           05  CDT-HOUR    PIC 9(2).
           05  CDT-MIN     PIC 9(2).
           05  CDT-SEC     PIC 9(2).

       01  WS-MSG        PIC X(133).

       PROCEDURE DIVISION.
       MAIN-SECTION.
           PERFORM INIT
           PERFORM COPY-LOOP
           PERFORM REPORT
           PERFORM TIDY-UP
           GOBACK.

       INIT.
           OPEN INPUT  INFILE
                OUTPUT OUTFILE
                OUTPUT PRTFILE
           IF IN-STAT  NOT = "00" AND NOT = "97"
              MOVE "OPEN ERROR ON SYSUT1, STATUS=" TO WS-MSG
              STRING WS-MSG DELIMITED BY SIZE
                     IN-STAT  DELIMITED BY SIZE
                     INTO PRTLINE
              WRITE PRTLINE
              STOP RUN
           END-IF
           IF OUT-STAT NOT = "00" AND NOT = "97"
              MOVE "OPEN ERROR ON SYSUT2, STATUS=" TO WS-MSG
              STRING WS-MSG DELIMITED BY SIZE
                     OUT-STAT DELIMITED BY SIZE
                     INTO PRTLINE
              WRITE PRTLINE
              STOP RUN
           END-IF
           IF PRT-STAT NOT = "00" AND NOT = "97"
              STOP RUN
           END-IF
           MOVE 0 TO WS-COUNT
           MOVE 'N' TO WS-EOF.

       COPY-LOOP.
           PERFORM UNTIL WS-EOF = 'Y'
              READ INFILE
                 AT END
                    MOVE 'Y' TO WS-EOF
                 NOT AT END
                    ADD 1 TO WS-COUNT
                    MOVE INREC(1:IN-LEN) TO OUTREC(1:IN-LEN)
                    MOVE IN-LEN TO OUT-LEN
                    WRITE OUTREC
                    IF OUT-STAT NOT = "00"
                       MOVE "WRITE ERROR ON SYSUT2, STATUS=" TO WS-MSG
                       STRING WS-MSG DELIMITED BY SIZE
                              OUT-STAT DELIMITED BY SIZE
                              INTO PRTLINE
                       WRITE PRTLINE
                       STOP RUN
                    END-IF
              END-READ
           END-PERFORM.

       REPORT.
           ACCEPT CURR-DATE-TIME FROM CURRENT-DATE
           MOVE CDYMD(CDT-YEAR, CDT-MONTH, CDT-DAY)  TO WS-CURR-DT
           MOVE CHTIME(CDT-HOUR, CDT-MIN, CDT-SEC)   TO WS-CURR-TM
           STRING "COBGENER COMPLETED  "
                  WS-CURR-DT DELIMITED BY SIZE
                  " "        DELIMITED BY SIZE
                  WS-CURR-TM DELIMITED BY SIZE
                  "  RECORDS COPIED=" DELIMITED BY SIZE
                  FUNCTION NUMVAL-C(WS-COUNT)  DELIMITED BY SIZE
                  INTO PRTLINE
           END-STRING
           WRITE PRTLINE.

       TIDY-UP.
           CLOSE INFILE OUTFILE PRTFILE
           EXIT.

       DECLARATIVES.
       END DECLARATIVES.

       LOCAL-STORAGE SECTION.
       LINKAGE SECTION.

       FUNCTION-ID. CDYMD.
       DATA DIVISION.
       LINKAGE SECTION.
       01 LY PIC 9(4).
       01 LM PIC 9(2).
       01 LD PIC 9(2).
       01 RTN PIC X(10).
       PROCEDURE DIVISION USING LY LM LD RETURNING RTN.
           STRING LY DELIMITED BY SIZE
                  "-" DELIMITED BY SIZE
                  LM DELIMITED BY SIZE
                  "-" DELIMITED BY SIZE
                  LD DELIMITED BY SIZE
                  INTO RTN
           GOBACK.
       END FUNCTION CDYMD.

       FUNCTION-ID. CHTIME.
       DATA DIVISION.
       LINKAGE SECTION.
       01 LH PIC 9(2).
       01 LN PIC 9(2).
       01 LS PIC 9(2).
       01 TRT PIC X(8).
       PROCEDURE DIVISION USING LH LN LS RETURNING TRT.
           STRING LH DELIMITED BY SIZE
                  ":" DELIMITED BY SIZE
                  LN DELIMITED BY SIZE
                  ":" DELIMITED BY SIZE
                  LS DELIMITED BY SIZE
                  INTO TRT
           GOBACK.
       END FUNCTION CHTIME.