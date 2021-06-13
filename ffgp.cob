       IDENTIFICATION DIVISION.
       PROGRAM-ID.   FFGP.
       AUTHOR.       CHIPMAN.
       
      *****************************************************************
      * FIND F GIVEN P                                                *
      *                                                               *
      * A simple program that calculates the Future Value of an       *
      * investment after the user enters the Present Value, the       *
      * annual interest rate, and the term of the investment.         *
      *                                                               *
      *****************************************************************

       DATA DIVISION.
       WORKING-STORAGE SECTION.

      *****************************************************************
      *                                                               *
      * Future Value formula:                                         *
      *                                                               *
      *              F = P * (1+i)^n                                  *
      *                                                               *
      *       P = Present Value                                       *
      *       F = Future Value                                        *
      *       i = annual-interest (rate)                              *
      *       n = loan-term (years)                                   *
      *                                                               *
      *****************************************************************

       01 INP-FIELDS.
           05 PV-IN             PIC X(8).
           05 INT-IN            PIC X(5).
           05 TERM-IN           PIC X(4).

       01 CALC-FIELDS.
           05 PV                PIC 9(5)V99.
           05 ANNUAL-INTEREST   PIC 99V9(4).
           05 ANNUAL-TERM       PIC 99.
           05 NUMERATOR         PIC S9(9)V9(6).
           05 DENOMINATOR       PIC S9(9)V9(6).
           05 FUTURE-VALUE      PIC S9(9)V99.

       01 DISP-FIELDS.
           05 PV-OUT            PIC $ZZ,ZZ9.99.
           05 FV-OUT            PIC $ZZZ,ZZZ,ZZ9.99.
           05 INTEREST-RATE     PIC Z9.99.
           05 TERM-OUT          PIC Z9.

       PROCEDURE DIVISION.
       100-MAIN-PARA.
           PERFORM 110-OPENING-SCREEN THRU 160-DISP-RESULT.
       
       999-END-PROGRAM.
           DISPLAY SPACES
           DISPLAY "***************** END-PROGRAM *****************"
           STOP RUN.
       
       110-OPENING-SCREEN.
           DISPLAY SPACES
           DISPLAY "***********************************************"
           DISPLAY "            FUTURE VALUE CALCULATOR"
           DISPLAY "***********************************************"
           display "Written by, Clifford A. Chipman, EMIT"
           display "August 16, 2020"
           display spaces
           display "in VSI COBOL for OpenVMS".
              
       120-PV-IN.
           DISPLAY SPACES
           DISPLAY "Enter zero for any parameter to end the program."
           DISPLAY SPACES
           DISPLAY "Enter present value: " WITH NO ADVANCING
           ACCEPT PV-IN
           MOVE FUNCTION LOWER-CASE(PV-IN) TO PV-IN

           IF PV-IN IS EQUAL TO "zero"
           THEN GO TO 999-END-PROGRAM
           ELSE COMPUTE PV = FUNCTION NUMVAL(PV-IN)
           END-IF
           
           IF PV IS EQUAL TO ZERO
           THEN GO TO 999-END-PROGRAM
           END-IF
           
           IF PV > 99999.99 THEN
           DISPLAY "Present value must be <= $99,999.99"
           GO TO 120-PV-IN
           END-IF.
           
       130-INT-IN.
           DISPLAY SPACES
           DISPLAY "Enter annual interest rate as %: " WITH NO ADVANCING
           ACCEPT INT-IN
           MOVE FUNCTION LOWER-CASE(INT-IN) TO INT-IN

           IF INT-IN IS EQUAL TO "zero"
           THEN GO TO 999-END-PROGRAM
           ELSE COMPUTE ANNUAL-INTEREST = FUNCTION NUMVAL(INT-IN)
           END-IF
           
           IF ANNUAL-INTEREST IS EQUAL TO ZERO
           THEN GO TO 999-END-PROGRAM
           END-IF
                      
           IF ANNUAL-INTEREST > 26 THEN
            DISPLAY "Interest must be <= 26%"
            GO TO 130-INT-IN
           END-IF
           
           DIVIDE 100 INTO ANNUAL-INTEREST.

       140-TERM-IN.
           DISPLAY SPACES
           DISPLAY "Enter term in years: " WITH NO ADVANCING
           ACCEPT TERM-IN
           MOVE FUNCTION LOWER-CASE(TERM-IN) TO TERM-IN
           
           IF TERM-IN IS EQUAL TO "zero"
           THEN GO TO 999-END-PROGRAM
           ELSE COMPUTE ANNUAL-TERM = FUNCTION NUMVAL(TERM-IN)
           END-IF
           
           IF ANNUAL-TERM IS EQUAL TO ZERO
           THEN GO TO 999-END-PROGRAM
           END-IF
                      
           IF ANNUAL-TERM > 30
            DISPLAY "Term must be <= 30 years."
            GO TO 140-TERM-IN
           END-IF.

       150-CALCULATE-IT.

      *****************************************************************
      *                                                               *
      * Future Value formula:                                         *
      *                                                               *
      *              F = P * (1+i)^n                                  *
      *                                                               *
      *       P = Present Value                                       *
      *       F = Future Value                                        *
      *       i = annual-interest (rate)                              *
      *       n = loan-term (years)                                   *
      *                                                               *
      *****************************************************************

           COMPUTE DENOMINATOR = (1 + ANNUAL-INTEREST) ** ANNUAL-TERM
           COMPUTE FUTURE-VALUE = PV * DENOMINATOR

           MULTIPLY 100 BY ANNUAL-INTEREST

           MOVE ANNUAL-INTEREST TO INTEREST-RATE
           MOVE ANNUAL-TERM TO TERM-OUT
           MOVE PV TO PV-OUT
           MOVE FUTURE-VALUE TO FV-OUT.

       160-DISP-RESULT.
           DISPLAY SPACES
           DISPLAY "Present Value: " pv-out
           DISPLAY "Term: " term-out " years"
           DISPLAY "Interest Rate: " interest-rate "%"
           DISPLAY "Your investment will be worth: " fv-out
           DISPLAY SPACES.
