       IDENTIFICATION DIVISION.
       PROGRAM-ID.  ADD2NUMS.
       AUTHOR.      CHIPMAN.
      *
      * This version of ADD2NUMS will allow the user to enter the
      * numbers without leading zeros.
      *      
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      
       01 INPUT-ONE           PIC X(4) VALUE SPACES.
       01 INPUT-TWO           PIC X(4) VALUE SPACES.
      
       01 FIRST-NUMBER        PIC 9(4) VALUE ZERO.
       01 SECOND-NUMBER       PIC 9(4) VALUE ZERO.
       01 ANSWER              PIC 9(5) VALUE ZERO.
      
       01 ANSWER-OUT          PIC ZZZZ9.
      
       PROCEDURE DIVISION.
       ONLY-PARA.
           DISPLAY "Enter 1st number: " WITH NO ADVANCING.
           ACCEPT INPUT-ONE.
           COMPUTE FIRST-NUMBER = FUNCTION NUMVAL(INPUT-ONE).
           DISPLAY "Enter 2nd number: " WITH NO ADVANCING.
           ACCEPT INPUT-TWO.
           COMPUTE SECOND-NUMBER = FUNCTION NUMVAL (INPUT-TWO).
           ADD FIRST-NUMBER TO SECOND-NUMBER GIVING ANSWER.
           MOVE ANSWER TO ANSWER-OUT.
           DISPLAY "The answer is: " WITH NO ADVANCING.
           DISPLAY ANSWER-OUT.
           STOP RUN.
