      *****************************************************************
      * FIND A GIVEN P                                                *
      *                                                               *
      * A simple program that calculates the Annual Uniform Series of *
      * an investment after the user enters the Present Value. the    *
      * annual interest rate, and the term of the investment.         *
      *                                                               *
      *****************************************************************

       identification division.
       program-id.   fagp.

       data division.
       working-storage section.

      *****************************************************************
      *                                                               *
      * Annual Worth formula:                                         *
      *                                                               *
      *              A = P * (i * (1+i)^n) / ((1+i)^n -1)             *
      *                                                               *
      *       P = Present Value                                       *
      *       A = Annual Worth                                        *
      *       i = annual-interest (rate)                              *
      *       n = loan-term (years)                                   *
      *                                                               *
      *****************************************************************

      * Data-entry-fields.
       01 PV-IN              pic x(8).
       01 INT-IN             pic x(5).
       01 TERM-IN            pic x(4).

      * Calculated-fields.
       01 PV                 pic 9(5)v99.
       01 ANNUAL-INTEREST    pic 99V9(4).
       01 ANNUAL-TERM        pic 99.
       01 NUMERATOR          pic 9(9)V9(6).
       01 DENOMINATOR        pic 9(9)V9(6).
       01 ANNUAL-WORTH       pic 9(9)V99.

      * Displayed-fields.
       01 PV-OUT             pic $ZZ,ZZ9.99 USAGE DISPLAY.
       01 AW-OUT             pic $ZZZ,ZZZ,ZZ9.99 USAGE DISPLAY.
       01 INTEREST-RATE      pic Z9.99 USAGE DISPLAY.
       01 TERM-OUT           pic Z9 USAGE DISPLAY.

      * Constant-values.
       01 min-val            pic 9(7)v99 value 0.01.
       01 max-val            pic 9(7)v99 value 9999999.99.
       01 min-int            pic 99v99 value 0.01.
       01 max-int            pic 99v99 value 26.
       01 min-term           pic 99 value 1.
       01 max-term           pic 99 value 30.

      * Constant-text.
       01 not-numeric        pic x(16) value " is NOT numeric.".
       01 quantity-too-small pic x(22)
                             value "Value must be >= $0.01".
       01 quantity-too-much  pic x(27)
                             value "Value must be <= $9,999,999".
       01 interest-too-much  pic x(23)
                             value "Interest must be <= 26%".
       01 interest-too-small pic x(21)
                             value "Interest must be > 0%".
       01 term-too-short     pic x(29)
                             value "Term must be at least 1 year.".
       01 term-too-long      pic x(25)
                             value "Term must be <= 30 years.".

       procedure division.
       100-MAIN-PARA.
           PERFORM 110-DISPLAY-TITLE-SCREEN THRU 160-DISP-RESULT.

       999-end-program.
           display spaces
           display "***** ANNUAL WORTH CALCULATOR UTILITY ENDS *****"
           display spaces
           stop run.

       110-display-title-screen.
           display spaces
           display "***** ANNUAL WORTH CALCULATOR UTILITY BEGINS *****"
           display "Written by, Clifford A. Chipman, EMIT"
           display "August 16, 2020"
           display spaces
           display "in VSI COBOL for OpenVMS".

       120-present-value-data-entry.
           display spaces
           display "Enter zero for any parameter to end the program."
           display spaces
           display "Enter present value: " with no advancing
           accept pv-in
           move function LOWER-CASE (pv-in) to PV-IN 

           if pv-in IS EQUAL TO "zero"
            then go to 999-end-program 
           else
              compute PV = function numval(pv-in)
           end-if

           if PV IS EQUAL ZERO then
              go to 999-end-program
           end-if

           if PV > max-val
              display quantity-too-much
              display spaces
              go to 120-present-value-data-entry
           end-if

           if PV < min-val
              display quantity-too-small
              display spaces
              go to 120-present-value-data-entry
           end-if

           move pv to pv-out.

       130-interest-rate-data-entry.
           display "Enter annual interest rate %: " with no advancing
           accept int-in
           MOVE FUNCTION LOWER-CASE (INT-IN) TO INT-IN 

           if int-in IS EQUAL TO "zero"
            then GO TO 999-end-program 
           else
              compute annual-interest = function numval(int-in)
           end-if

           if annual-interest IS EQUAL ZERO then
              go to 999-end-program
           end-if

           if annual-interest > max-int
              display interest-too-much
              display spaces
              go to 130-interest-rate-data-entry
           end-if

           if annual-interest IS LESS THAN ZERO then
              display interest-too-small
              display spaces
              go to 130-interest-rate-data-entry
           end-if

           move annual-interest to interest-rate
           divide 100 INTO annual-interest.

       140-term-data-entry.
           display "Enter term in years: " with no advancing
           accept term-in
           move function LOWER-CASE (term-in) to TERM-IN 

           if term-in IS EQUAL to "zero"
            then go to 999-end-program
           else
              compute annual-term = function numval(term-in)
           end-if

           if annual-term IS EQUAL ZERO then
              go to 999-end-program
           end-if

           if annual-term > max-term then
              display term-too-long
              display spaces
              go to 140-term-data-entry
           end-if

           if annual-term < min-term then
              display term-too-short
              display spaces
              go to 140-term-data-entry
           end-if

           move annual-term to term-out.

       150-calculate-it.

      *****************************************************************
      *                                                               *
      * Annual Worth formula:                                         *
      *                                                               *
      *              A = P * (i * (1+i)^n) / ((1+i)^n -1)             *
      *                                                               *
      *       P = Present Value                                       *
      *       A = Annual Worth                                        *
      *       i = annual-interest (rate)                              *
      *       n = loan-term (years)                                   *
      *                                                               *
      *****************************************************************

      *     compute denominator = (1 + annual-interest) **
      *                             annual-term - 1
           add 1 to ANNUAL-INTEREST 
           compute denominator = ANNUAL-INTEREST ** ANNUAL-TERM 
           subtract 1 from DENOMINATOR 

      *     compute numerator = annual-interest *
      *                           (1 + annual-interest) ** annual-term

           compute NUMERATOR  = ANNUAL-INTEREST ** ANNUAL-TERM 
           subtract 1 from ANNUAL-INTEREST 
           MULTIPLY ANNUAL-INTEREST BY NUMERATOR 

           compute ANNUAL-WORTH  = PV * (numerator / denominator)

           move annual-worth to aw-out.

       160-DISP-RESULT.

           display spaces
           display "Your initial investment of: " pv-out
           display "Term: " term-out " years"
           display "Interest Rate: " interest-rate "%"
           display "You can annually withdraw: " aw-out.

