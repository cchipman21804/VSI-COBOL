       identification division.
       program-id.    hafwvant.
       author.        Chipman.
      *
      * Calculate the dimensions of and the resonant frequency of
      * a half-wave dipole antenna's elements
      *
      *         468
      * F = ------------
      *          L
      *
      *         468
      * L = ------------
      *          F
      *
       data division.
       working-storage section.
      *
       01 user-input         pic x(10).
       01 menu-selection     pic 9.
      *
      * Calculation variables.
       01 dividend           pic 999       value 468.
       01 divisor            pic 999v9(6).
       01 quotient           pic 999v9(6).
      *
      * Constant-values.
       01 min-menu           pic 9         value zero.
       01 max-menu           pic 9         value 2.
      *
       01 min-length         pic 9v99      value 1.04.
       01 max-length         pic 999v9(6)  value 999.999999.
      *
       01 min-frequency      pic 9v9(6)    value 3.5.
       01 max-frequency      pic 999       value 450.
      *
       01 frequency-prompt   pic x(9)
                             value "Frequency".
      *
       01 frequency-unit     pic x(3)
                             value "MHz".
      *
       01 length-prompt      pic x(6)
                             value "Length".
      *
       01 length-unit        pic x(2)
                             value "ft".
      *
       01 too-small          pic x(14)
                             value " is too small.".
      *
       01 too-much           pic x(13)
                             value " is too much.".
      *
      * function specific values (filled from constant values).
       01 input-prompt       pic x(9).
       01 input-unit         pic x(3).
       01 other-unit         pic x(3).
       01 min-val            pic 9v9(6).
       01 max-val            pic 999v9(6).
      *
      * Display variables.
       01 quotient-out       pic zz9.9(6).
       01 min-val-out        pic zz9.9(6).
       01 max-val-out        pic zz9.9(6).
      * 
       procedure division.
      *
      * Calculate the dimensions of and the resonant frequency of
      * a half-wave dipole antenna's elements
      *
      *         468
      * F = ------------
      *          L
      *
      *         468
      * L = ------------
      *          F
      *
       100-main.
           perform 110-opening-screen
           perform 120-main-menu
           perform 130-data-entry.
      *
       999-end-program.
           stop run.
      *
       110-opening-screen.
           display spaces
           display "Half-Wave Dipole Calculations"
           display "Written by, Clifford A. Chipman, EMIT"
           display "February 23, 2021"
           display spaces
           display "in VS COBOL for OpenVMS".
      *
       120-main-menu.
           display spaces
           display " 1 --- Calculate resonant frequency from length"
           display " 2 --- Calculate length from resonant frequency"
           display " 0 --- Quit"
           display "Select: " with no advancing
           accept user-input
           move function lower-case(user-input) to user-input
      *
           if user-input is equal to "zero" then
              go to 999-end-program
           else
              compute menu-selection = function numval(user-input)
           end-if
      *
           evaluate menu-selection
              when 0
                   go to 999-end-program
      *
              when 1
                   move length-prompt    to input-prompt
                   move length-unit      to input-unit
                   move frequency-unit   to other-unit
                   move min-length       to min-val
                   move max-length       to max-val
      *
              when 2
                   move frequency-prompt to input-prompt
                   move frequency-unit   to input-unit
                   move length-unit      to other-unit
                   move min-frequency    to min-val
                   move max-frequency    to max-val
      *
              when other
                   go to 120-main-menu
           end-evaluate.
      *
       130-data-entry.
           move min-val to min-val-out
           move max-val to max-val-out
           display spaces
           display min-val-out " - " max-val-out
           display "Enter " input-prompt " in " input-unit ": "
                   with no advancing
           accept user-input
           move function lower-case(user-input) to user-input
      *
           if user-input is equal to "zero" then
              go to 999-end-program
           else compute divisor = function numval(user-input)
           end-if
      *
           evaluate true
              when divisor is less than min-val
                   display spaces
                   display input-prompt too-small
                   go to 130-data-entry
      *
              when divisor is greater than max-val
                   display spaces
                   display input-prompt too-much
                   go to 130-data-entry
      *
              when other
                   divide dividend by divisor giving quotient rounded
                   move quotient to quotient-out
                   display quotient-out " " other-unit
           end-evaluate.
