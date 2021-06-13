      * BigNum -- a game by Dan Sanderson (aka Doc Brown)
      *
      * Ported to COBOL by Cliff Chipman
      *
      * BigNum is a fairly simple game.  You have five places to put
      * digits to construct a five digit number.  These digits are
      * picked randomly (from 0 to 9), one by one.  As they are
      * picked, you choose where to put it.  Once you place the digit,
      * you can't move it.  The goal is to construct the largest possible
      * number with those digits.
      *
      * The board looks like this:
      *
      *        a   b   c   d   e
      *      ---------------------
      *      |   |   |   |   |   |
      *      ---------------------
      *
      * To place a digit, simply press the letter.
      *
      * If you get the largest number possible, you get back twice your
      * bet!  If you get the first digit the largest, you get 25% of
      * your bet back.
      *
       identification division.
       program-id.    bignum.
       author.        Chipman.
      *
       data division.
       working-storage section.
      *
      * Input variables.
       01 bet-in                          pic x(4).
       01 player-in                       pic x(4).
      *
      * Calculation variables.
       01 bet                             pic 9(4) value zero.
       01 minimum-bet                     pic 999  value 200.
       01 wallet                          pic 9(4) value 1000.
       01 reward                          pic 9(4) value zero.
      *
       01 table-indices.
          02 board-idx                    pic 9.
          02 table-idx                    pic 9.
      *
       01 tables.
          02 biggest-num occurs 5 times   pic 9.
          02 player-num  occurs 5 times   pic x    value space.
          02 board-num   occurs 5 times   pic 9.
      *
      * minutes, seconds, and hundredths-second from datetime function
      * helps randomize the generated random number
       01 datetime.
          02 yyyy                         pic 9(4).
          02 mo                           pic 99.
          02 dd                           pic 99.
          02 hh                           pic 99.
          02 mi                           pic 99.
          02 ss                           pic 99.
          02 hs                           pic 99.
          02 filler                       pic 9(5).
      *
      * Used for debugging
       01 datetime-label.
          02 filler                       pic x(4) value "yyyy".
          02 filler                       pic xx   value "mo".
          02 filler                       pic xx   value "dd".
          02 filler                       pic xx   value "hh".
          02 filler                       pic xx   value "mm".
          02 filler                       pic xx   value "ss".
          02 filler                       pic xx   value "hs".
      *
      * factors help further randomize the generated random number
       01 factor                          pic 9(5)v9(5).
       01 dt-factor.
          02 f-m                          pic 9.
          02 f-s                          pic 99.
          02 f-h                          pic 99.
      * 
      * Used for debugging
       01 factor-label.
          02 filler                       pic x(4) value spaces.
          02 filler                       pic x(12)
                                          value "|  factor  |".
      *
      * random-num stores the generated random number
       01 random-num                      pic 9v9(5).
      *
      * random-int stores the individual random integers
       01 random-int                      pic 9.
      *
       01 numbers.
          02 playernum                    pic 9(5) value zero.
          02 biggestnum                   pic 9(5) value zero.
      *
      * Binary flags
       01 continue-flag                   pic x.
          88 skip                                  value "n".
      *                                                  
       01 play-again-flag                 pic x.
          88 playagain                             value "y".
          88 exitpgm                               value "n".
      *
       01 enough-money-flag               pic x.
          88 enough-money                          value "y".
          88 not-enough-money                      value "n".
      *
       01 go-back-flag                    pic x.
          88 go-back                               value "y".
      *
       01 occupied-flag                   pic x.
          88 occupied                              value "y".
          88 not-occupied                          value "n".
      *
      * Constant message strings
       01 occupied-msg                    pic x(32)
                                 value "There is already a number there.".
      *
       01 fortystars                      pic x(40)
                         value "****************************************".
 
