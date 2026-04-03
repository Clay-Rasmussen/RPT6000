       IDENTIFICATION DIVISION.

       PROGRAM-ID. RPT6000.
      *****************************************************************
      *  Programmers: Clay Rasmussen
      *  Date.......: April **, 2025
      *  GitHub URL.:
      *  Description:
      *****************************************************************

       ENVIRONMENT DIVISION.

       INPUT-OUTPUT SECTION.

       FILE-CONTROL.
           SELECT INPUT-CUSTMAST ASSIGN TO CUSTMAST.
           SELECT OUTPUT-RPT6000 ASSIGN TO RPT6000.

       DATA DIVISION.
       FILE SECTION.
       FD  INPUT-CUSTMAST
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           RECORD CONTAINS 130 CHARACTERS
           BLOCK CONTAINS 130 CHARACTERS.

       01  CUSTOMER-MASTER-RECORD.
           05  CM-BRANCH-NUMBER        PIC 9(2).
           05  CM-SALESREP-NUMBER      PIC 9(2).
           05  CM-CUSTOMER-NUMBER      PIC 9(5).
           05  CM-CUSTOMER-NAME        PIC X(20).
           05  CM-SALES-THIS-YTD       PIC S9(5)V9(2).
           05  CM-SALES-LAST-YTD       PIC S9(5)V9(2).
           05  FILLER                  PIC X(87).

       FD  OUTPUT-RPT6000
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           RECORD CONTAINS 130 CHARACTERS
           BLOCK CONTAINS 130 CHARACTERS.

       01  PRINT-AREA      PIC X(130).

       WORKING-STORAGE SECTION.

       01 SALESREP-TABLE  VALUE "10DJOHNSON  11JSMITH    12TTHOMAS   14B
      -    "JONES    17CRASMUSSEN".
           05 SALESREP-GROUP OCCURS 6 TIMES
                             INDEXED BY SRT-INDEX.
              10  SALESREP-NUMBER PIC 9(2).
              10  SALESREP-NAME   PIC X(10).


       01  SWITCHES.
           05  CUSTMAST-EOF-SWITCH     PIC X    VALUE "N".
              88 CUSTMAST-EOF                   VALUE "Y".
           05  FIRST-RECORD-SWITCH     PIC X    VALUE "Y".
              88 NOT-FIRST-RECORD               VALUE "N".

       01  CONTROL-FIELDS.
           05  OLD-SALESREP-NUMBER     PIC 99.
           05  OLD-BRANCH-NUMBER       PIC 99.

       01  PRINT-FIELDS  PACKED-DECIMAL.
           05  PAGE-COUNT      PIC S9(3)   VALUE ZERO.
           05  LINES-ON-PAGE   PIC S9(3)   VALUE +55.
           05  LINE-COUNT      PIC S9(3)   VALUE +99.
           05  SPACE-CONTROL   PIC S9.

       01  TOTAL-FIELDS  PACKED-DECIMAL.
           05 SALESREP-TOTAL-THIS-YTD PIC S9(6)V99   VALUE ZERO.
           05 SALESREP-TOTAL-LAST-YTD PIC S9(6)V99   VALUE ZERO.
           05 BRANCH-TOTAL-THIS-YTD   PIC S9(6)V99   VALUE ZERO.
           05 BRANCH-TOTAL-LAST-YTD   PIC S9(6)V99   VALUE ZERO.
           05 GRAND-TOTAL-THIS-YTD    PIC S9(7)V99   VALUE ZERO.
           05 GRAND-TOTAL-LAST-YTD    PIC S9(7)V99   VALUE ZERO.
           05 GRAND-TOTAL-CHANGE-AMT  PIC S9(7)V99   VALUE ZERO.
           05 GRAND-TOTAL-CHANGE-PCT  PIC S9(3)V9    VALUE ZERO.

       01  CALCULATION-FIELDS  PACKED-DECIMAL.
           05  WS-CHANGE-AMOUNT       PIC S9(7)V99   VALUE ZERO.
           05  WS-CHANGE-PERCENT      PIC S9(3)V9    VALUE ZERO.

       01  CURRENT-DATE-AND-TIME.
           05  CD-YEAR         PIC 9999.
           05  CD-MONTH        PIC 99.
           05  CD-DAY          PIC 99.
           05  CD-HOURS        PIC 99.
           05  CD-MINUTES      PIC 99.
           05  FILLER          PIC X(9).

       01  HEADING-LINE-1.
           05  FILLER          PIC X(7)    VALUE "DATE:  ".
           05  HL1-MONTH       PIC 9(2).
           05  FILLER          PIC X(1)    VALUE "/".
           05  HL1-DAY         PIC 9(2).
           05  FILLER          PIC X(1)    VALUE "/".
           05  HL1-YEAR        PIC 9(4).
           05  FILLER          PIC X(25)   VALUE SPACE.
           05  FILLER          PIC X(20)   VALUE "YEAR-TO-DATE SALES R".
           05  FILLER          PIC X(5)    VALUE "EPORT".
           05  FILLER          PIC X(26)   VALUE SPACE.
           05  FILLER          PIC X(11)    VALUE "     PAGE: ".
           05  HL1-PAGE-NUMBER PIC ZZZ9.

       01  HEADING-LINE-2.
           05  FILLER          PIC X(7)    VALUE "TIME:  ".
           05  HL2-HOURS       PIC 9(2).
           05  FILLER          PIC X(1)    VALUE ":".
           05  HL2-MINUTES     PIC 9(2).
           05  FILLER          PIC X(89)   VALUE SPACE.
           05  FILLER          PIC X(7)   VALUE "RPT6000".

       01  HEADING-LINE-3.
           05  FILLER PIC X(8)  VALUE "BRANCH  ".
           05  FILLER PIC X(25) VALUE "SALES SALESREP    CUST   ".
           05  FILLER PIC X(23) VALUE "SALES                  ".
           05  FILLER PIC X(16) VALUE "  SALES    ".
           05  FILLER PIC X(15) VALUE "CHANGE         ".
           05  FILLER PIC X(6)  VALUE "CHANGE".
           05  FILLER PIC X(44) VALUE SPACE.

       01  HEADING-LINE-4.
           05  FILLER PIC X(8)   VALUE " NUM    ".
           05  FILLER PIC X(23) VALUE " REP    NAME      NUM  ".
           05  FILLER PIC X(25) VALUE "  CUSTOMER NAME          ".
           05  FILLER PIC X(16) VALUE "  THIS YTD      ".
           05  FILLER PIC X(14) VALUE "LAST YTD      ".
           05  FILLER PIC X(13) VALUE " AMOUNT      ".
           05  FILLER PIC X(8)  VALUE " PERCENT".
           05  FILLER PIC X(43) VALUE SPACE.

       01  HEADING-LINE-5.
           05  FILLER PIC X(6)  VALUE ALL "-".
           05  FILLER PIC X(2)  VALUE SPACE.
           05  FILLER PIC X(5)  VALUE ALL "-".
           05  FILLER PIC X(1)  VALUE SPACE.
           05  FILLER PIC X(10) VALUE ALL "-".
           05  FILLER PIC X(2)  VALUE SPACE.
           05  FILLER PIC X(5)  VALUE ALL "-".
           05  FILLER PIC X(2)  VALUE SPACE.
           05  FILLER PIC X(22) VALUE ALL "-".
           05  FILLER PIC X(2)  VALUE SPACE.
           05  FILLER PIC X(10) VALUE ALL "-".
           05  FILLER PIC X(4)  VALUE SPACE.
           05  FILLER PIC X(10) VALUE ALL "-".
           05  FILLER PIC X(4)  VALUE SPACE.
           05  FILLER PIC X(10) VALUE ALL "-".
           05  FILLER PIC X(4)  VALUE SPACE.
           05  FILLER PIC X(9)  VALUE ALL "-".
           05  FILLER PIC X(38) VALUE SPACE.

       01  CUSTOMER-LINE.
           05  FILLER              PIC X(1)    VALUE SPACE.
           05  CL-BRANCH-NUMBER    PIC X(2).
           05  FILLER              PIC X(6)    VALUE SPACE.
           05  CL-SALESREP-NUMBER  PIC X(2).
           05  FILLER              PIC X(3)    VALUE SPACE.
           05  CL-SALESREP-NAME    PIC X(10).
           05  FILLER              PIC X(2)    VALUE SPACE.
           05  CL-CUSTOMER-NUMBER  PIC 9(5).
           05  FILLER              PIC X(2)    VALUE SPACE.
           05  CL-CUSTOMER-NAME    PIC X(20).
           05  FILLER              PIC X(4)    VALUE SPACE.
           05  CL-SALES-THIS-YTD   PIC ZZ,ZZ9.99-.
           05  FILLER              PIC X(4)    VALUE SPACE.
           05  CL-SALES-LAST-YTD   PIC ZZ,ZZ9.99-.
           05  FILLER              PIC X(4)    VALUE SPACE.
           05  CL-CHANGE-AMOUNT    PIC ZZ,ZZ9.99-.
           05  FILLER              PIC X(4)    VALUE SPACE.
           05  CL-CHANGE-PERCENT   PIC ---9.9.
           05  CL-CHANGE-PERCENT-R REDEFINES CL-CHANGE-PERCENT
                                   PIC X(6).
           05  FILLER              PIC X(42)   VALUE SPACE.

       01  SALESREP-TOTAL-LINE.
           05  FILLER              PIC X(41)   VALUE SPACE.
           05  FILLER              PIC X(14)   VALUE "SALESREP TOTAL".
           05  FILLER              PIC X(1)    VALUE SPACE.
           05  STL-SALES-THIS-YTD  PIC $$$,$$9.99-.
           05  FILLER              PIC X(3)    VALUE SPACE.
           05  STL-SALES-LAST-YTD  PIC $$$,$$9.99-.
           05  FILLER              PIC X(3)    VALUE SPACE.
           05  STL-CHANGE-AMOUNT   PIC $$$,$$9.99-.
           05  FILLER              PIC X(4)    VALUE SPACE.
           05  STL-CHANGE-PERCENT  PIC +++9.9.
           05  STL-CHANGE-PERCENT-R REDEFINES STL-CHANGE-PERCENT
                                   PIC X(6).
           05  FILLER              PIC X(46)   VALUE "*".

       01  BRANCH-TOTAL-LINE.
           05  FILLER              PIC X(41)   VALUE SPACE.
           05  FILLER              PIC X(15)   VALUE "BRANCH TOTAL   ".
           05  BTL-SALES-THIS-YTD  PIC $$$,$$9.99-.
           05  FILLER              PIC X(3)    VALUE SPACE.
           05  BTL-SALES-LAST-YTD  PIC $$$,$$9.99-.
           05  FILLER              PIC X(3)    VALUE SPACE.
           05  BTL-CHANGE-AMOUNT   PIC $$$,$$9.99-.
           05  FILLER              PIC X(4)    VALUE SPACE.
           05  BTL-CHANGE-PERCENT  PIC +++9.9.
           05  BTL-CHANGE-PERCENT-R REDEFINES BTL-CHANGE-PERCENT
                                   PIC X(6).
           05  FILLER              PIC X(48)   VALUE "**".

       01  GRAND-TOTAL-LINE-1.
           05  FILLER              PIC X(57)   VALUE SPACE.
           05  FILLER              PIC X(10)   VALUE ALL "=".
           05  FILLER              PIC X(4)    VALUE SPACE.
           05  FILLER              PIC X(10)   VALUE ALL "=".
           05  FILLER              PIC X(4)    VALUE SPACE.
           05  FILLER              PIC X(10)   VALUE ALL "=".
           05  FILLER              PIC X(4)    VALUE SPACE.
           05  FILLER              PIC X(9)    VALUE ALL "=".

       01  GRAND-TOTAL-LINE-2.
           05  FILLER              PIC X(41)   VALUE SPACE.
           05  FILLER              PIC X(12)   VALUE "GRAND TOTAL:".
           05  FILLER              PIC X(2)    VALUE SPACE.
           05  GTL-SALES-THIS-YTD  PIC Z,ZZZ,ZZ9.99-.
           05  FILLER              PIC X(1)    VALUE SPACE.
           05  GTL-SALES-LAST-YTD  PIC Z,ZZZ,ZZ9.99-.
           05  GTL-CHANGE-AMOUNT   PIC Z,ZZZ,ZZ9.99-.
           05  FILLER              PIC X(5)    VALUE SPACE.
           05  GTL-CHANGE-PERCENT  PIC ZZ9.9-.
           05  FILLER              PIC X(47)   VALUE "***".


      * This is the main control paragraph of the program. It opens the
      * input and output files, initializes the report heading,
      * processes all customer records until end-of-file, prints the
      * grand totals, and then closes the files before terminating
      * execution.
       PROCEDURE DIVISION.
       000-PREPARE-SALES-REPORT.
           OPEN INPUT  INPUT-CUSTMAST
                OUTPUT OUTPUT-RPT6000.
           PERFORM 100-FORMAT-REPORT-HEADING.
           PERFORM 300-PREPARE-SALES-LINES
               UNTIL CUSTMAST-EOF.
           PERFORM 500-PRINT-GRAND-TOTALS.
           CLOSE INPUT-CUSTMAST
                 OUTPUT-RPT6000.
           STOP RUN.


      * This paragraph retrieves the current system date and time using
      * the CURRENT-DATE function. It formats and moves the date and
      * time values into the heading fields used for printing the
      * report header.
       100-FORMAT-REPORT-HEADING.
           MOVE FUNCTION CURRENT-DATE TO CURRENT-DATE-AND-TIME.
           MOVE CD-MONTH   TO HL1-MONTH.
           MOVE CD-DAY     TO HL1-DAY.
           MOVE CD-YEAR    TO HL1-YEAR.
           MOVE CD-HOURS   TO HL2-HOURS.
           MOVE CD-MINUTES TO HL2-MINUTES.


      * This is the main processing loop for the report. It reads each
      * customer record and determines control breaks using an EVALUATE
      * statement. Based on changes in branch or sales representative,
      * it triggers printing of totals and customer lines accordingly.
       300-PREPARE-SALES-LINES.
           PERFORM 310-READ-CUSTOMER-RECORD.
           EVALUATE TRUE
              WHEN CUSTMAST-EOF
                PERFORM 355-PRINT-SALESREP-LINE
                PERFORM 360-PRINT-BRANCH-LINE
           WHEN FIRST-RECORD-SWITCH = "Y"
              PERFORM 320-PRINT-CUSTOMER-LINE
              MOVE "N" TO FIRST-RECORD-SWITCH
              MOVE CM-SALESREP-NUMBER TO OLD-SALESREP-NUMBER
              MOVE CM-BRANCH-NUMBER TO OLD-BRANCH-NUMBER
           WHEN CM-BRANCH-NUMBER > OLD-BRANCH-NUMBER
              PERFORM 355-PRINT-SALESREP-LINE
              PERFORM 360-PRINT-BRANCH-LINE
              PERFORM 320-PRINT-CUSTOMER-LINE
      *        MOVE CM-SALESREP-NUMBER TO OLD-SALESREP-NUMBER
      *        MOVE CM-BRANCH-NUMBER TO OLD-BRANCH-NUMBER
           WHEN CM-SALESREP-NUMBER > OLD-SALESREP-NUMBER
              PERFORM 355-PRINT-SALESREP-LINE
              PERFORM 320-PRINT-CUSTOMER-LINE
              MOVE CM-SALESREP-NUMBER TO OLD-SALESREP-NUMBER
           WHEN OTHER
            PERFORM 320-PRINT-CUSTOMER-LINE
            END-EVALUATE.


      * This paragraph reads the next record from the input file.
      * If the end of the file is reached, it sets the CUSTMAST-EOF
      * condition to stop further processing.
       310-READ-CUSTOMER-RECORD.
           READ INPUT-CUSTMAST
              AT END
                 SET CUSTMAST-EOF TO TRUE.


      * This paragraph formats and prints a single customer detail line.
      * It handles pagination, moves customer data into print fields,
      * calculates sales change amount and percentage, and writes the
      * formatted line to the report. It also updates branch totals.
       320-PRINT-CUSTOMER-LINE.

           IF LINE-COUNT >= LINES-ON-PAGE
              PERFORM 330-PRINT-HEADING-LINES.

           IF CM-BRANCH-NUMBER NOT = OLD-BRANCH-NUMBER
                MOVE CM-BRANCH-NUMBER TO CL-BRANCH-NUMBER
                PERFORM 325-MOVE-SALESREP-NAME
           ELSE
                MOVE SPACES TO CL-BRANCH-NUMBER.
                PERFORM 325-MOVE-SALESREP-NAME.


           IF CM-SALESREP-NUMBER NOT = OLD-SALESREP-NUMBER
              MOVE CM-SALESREP-NUMBER TO CL-SALESREP-NUMBER
              PERFORM 325-MOVE-SALESREP-NAME
           ELSE
              MOVE SPACES TO CL-SALESREP-NUMBER.
              PERFORM 325-MOVE-SALESREP-NAME.

           MOVE CM-CUSTOMER-NUMBER TO CL-CUSTOMER-NUMBER.
           MOVE CM-CUSTOMER-NAME TO CL-CUSTOMER-NAME.
           MOVE CM-SALES-THIS-YTD TO CL-SALES-THIS-YTD.
           MOVE CM-SALES-LAST-YTD TO CL-SALES-LAST-YTD.
           COMPUTE WS-CHANGE-AMOUNT =
              CM-SALES-THIS-YTD - CM-SALES-LAST-YTD.
           MOVE WS-CHANGE-AMOUNT TO CL-CHANGE-AMOUNT.
           IF CM-SALES-LAST-YTD = ZERO
              MOVE "  N/A " TO CL-CHANGE-PERCENT-R
           ELSE
              COMPUTE CL-CHANGE-PERCENT ROUNDED =
                 WS-CHANGE-AMOUNT * 100 / CM-SALES-LAST-YTD
                 ON SIZE ERROR
                    MOVE "OVRFLW" TO CL-CHANGE-PERCENT-R.
           MOVE CUSTOMER-LINE TO PRINT-AREA.
           PERFORM 350-WRITE-REPORT-LINE.
           MOVE 1 TO SPACE-CONTROL.
           ADD CM-SALES-THIS-YTD TO SALESREP-TOTAL-THIS-YTD.
           ADD CM-SALES-LAST-YTD TO SALESREP-TOTAL-LAST-YTD.
           MOVE CM-SALESREP-NUMBER TO OLD-SALESREP-NUMBER.
           MOVE CM-BRANCH-NUMBER TO OLD-BRANCH-NUMBER.

       325-MOVE-SALESREP-NAME.
           SET SRT-INDEX TO 1.
           SEARCH SALESREP-GROUP
              AT END
                 MOVE "UNKNOWN" TO CL-SALESREP-NAME
              WHEN SALESREP-NUMBER (SRT-INDEX) = CM-SALESREP-NUMBER
                 MOVE SALESREP-NAME (SRT-INDEX) TO CL-SALESREP-NAME
           END-SEARCH.


      * This paragraph handles page breaks and prints the report
      * headings. It increments the page number, prints all heading
      * lines, and resets the line counter for the new page.
       330-PRINT-HEADING-LINES.
           ADD 1 TO PAGE-COUNT.
           MOVE PAGE-COUNT     TO HL1-PAGE-NUMBER.
           MOVE HEADING-LINE-1 TO PRINT-AREA.
           PERFORM 340-WRITE-PAGE-TOP-LINE.
           MOVE HEADING-LINE-2 TO PRINT-AREA.
           PERFORM 350-WRITE-REPORT-LINE.
           MOVE HEADING-LINE-3 TO PRINT-AREA.
           MOVE 2 TO SPACE-CONTROL.
           PERFORM 350-WRITE-REPORT-LINE.
           MOVE HEADING-LINE-4 TO PRINT-AREA.
           MOVE 1 TO SPACE-CONTROL.
           PERFORM 350-WRITE-REPORT-LINE.
           MOVE HEADING-LINE-5 TO PRINT-AREA.
           MOVE 1 TO SPACE-CONTROL.
           PERFORM 350-WRITE-REPORT-LINE.
           MOVE ZERO TO LINE-COUNT.
           MOVE 2 TO SPACE-CONTROL.


      * This paragraph writes the first line of a new page (top line)
      * to the output file and initializes the line count.
       340-WRITE-PAGE-TOP-LINE.
           WRITE PRINT-AREA.
           MOVE 1 TO LINE-COUNT.


      * This is a utility paragraph used to write a single formatted
      * line (PRINT-AREA) to the output report file.
       350-WRITE-REPORT-LINE.
           WRITE PRINT-AREA.


      * This paragraph prints totals for a sales representative.
      * It calculates total sales, change amount, and percentage,
      * prints separator lines, outputs the sales representative totals,
      * and resets the sales representative accumulators. It also rolls
      * totals into the branch totals.
       355-PRINT-SALESREP-LINE.
           MOVE SALESREP-TOTAL-THIS-YTD TO STL-SALES-THIS-YTD.
           MOVE SALESREP-TOTAL-LAST-YTD TO STL-SALES-LAST-YTD.
           COMPUTE WS-CHANGE-AMOUNT =
              SALESREP-TOTAL-THIS-YTD - SALESREP-TOTAL-LAST-YTD.
           MOVE WS-CHANGE-AMOUNT TO STL-CHANGE-AMOUNT.
           IF SALESREP-TOTAL-LAST-YTD = ZERO
              MOVE "  N/A " TO STL-CHANGE-PERCENT-R
           ELSE
              COMPUTE STL-CHANGE-PERCENT ROUNDED =
                 WS-CHANGE-AMOUNT * 100 / SALESREP-TOTAL-LAST-YTD
                 ON SIZE ERROR
                    MOVE "OVRFLW" TO STL-CHANGE-PERCENT-R.

           MOVE SALESREP-TOTAL-LINE TO PRINT-AREA.
           MOVE 1 TO SPACE-CONTROL.
           PERFORM 350-WRITE-REPORT-LINE.

           MOVE 2 TO SPACE-CONTROL.
           ADD SALESREP-TOTAL-THIS-YTD TO BRANCH-TOTAL-THIS-YTD.
           ADD SALESREP-TOTAL-LAST-YTD TO BRANCH-TOTAL-LAST-YTD.
           INITIALIZE SALESREP-TOTAL-THIS-YTD
                      SALESREP-TOTAL-LAST-YTD.


      * This paragraph prints totals for a branch. It calculates total
      * sales, change amount, and percentage, prints separator lines,
      * outputs the branch totals, and resets the branch accumulators.
      * It also adds totals to the grand totals.
       360-PRINT-BRANCH-LINE.
           MOVE BRANCH-TOTAL-THIS-YTD TO BTL-SALES-THIS-YTD.
           MOVE BRANCH-TOTAL-LAST-YTD TO BTL-SALES-LAST-YTD.
           COMPUTE WS-CHANGE-AMOUNT =
              BRANCH-TOTAL-THIS-YTD - BRANCH-TOTAL-LAST-YTD.
           MOVE WS-CHANGE-AMOUNT TO BTL-CHANGE-AMOUNT.
           IF BRANCH-TOTAL-LAST-YTD = ZERO
              MOVE "  N/A " TO BTL-CHANGE-PERCENT-R
           ELSE
              COMPUTE BTL-CHANGE-PERCENT ROUNDED =
                 WS-CHANGE-AMOUNT * 100 / BRANCH-TOTAL-LAST-YTD
                 ON SIZE ERROR
                    MOVE "OVRFLW" TO BTL-CHANGE-PERCENT-R.
           MOVE BRANCH-TOTAL-LINE TO PRINT-AREA.
           MOVE 1 TO SPACE-CONTROL.
           PERFORM 350-WRITE-REPORT-LINE.

           MOVE SPACES TO PRINT-AREA.
           PERFORM 350-WRITE-REPORT-LINE.

           MOVE 2 TO SPACE-CONTROL.
           ADD BRANCH-TOTAL-THIS-YTD TO GRAND-TOTAL-THIS-YTD.
           ADD BRANCH-TOTAL-LAST-YTD TO GRAND-TOTAL-LAST-YTD.
           INITIALIZE BRANCH-TOTAL-THIS-YTD
                      BRANCH-TOTAL-LAST-YTD.


      * This paragraph calculates and prints the overall grand totals
      * for the report. It computes the total change amount and
      * percentage, then outputs the final summary lines at the end of
      * the report.
       500-PRINT-GRAND-TOTALS.
           MOVE GRAND-TOTAL-THIS-YTD TO GTL-SALES-THIS-YTD.
           MOVE GRAND-TOTAL-LAST-YTD TO GTL-SALES-LAST-YTD.
           COMPUTE WS-CHANGE-AMOUNT =
              GRAND-TOTAL-THIS-YTD - GRAND-TOTAL-LAST-YTD.
           MOVE WS-CHANGE-AMOUNT TO GTL-CHANGE-AMOUNT.
           IF GRAND-TOTAL-LAST-YTD = ZERO
              MOVE 999.9 TO GTL-CHANGE-PERCENT
           ELSE
              COMPUTE GTL-CHANGE-PERCENT ROUNDED =
                 WS-CHANGE-AMOUNT * 100 / GRAND-TOTAL-LAST-YTD
                 ON SIZE ERROR
                    MOVE 999.9 TO GTL-CHANGE-PERCENT.
           MOVE GRAND-TOTAL-LINE-1 TO PRINT-AREA.
           PERFORM 350-WRITE-REPORT-LINE.
           MOVE GRAND-TOTAL-LINE-2 TO PRINT-AREA.
           PERFORM 350-WRITE-REPORT-LINE.
