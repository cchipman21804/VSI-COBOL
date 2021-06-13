       identification division.
       program-id.    diginvar.
       author.        Chipman.
      *
      * PERFECT DIGITAL INVARIANT
      *
      * HERE'S THE PROBLEM...
      *
      * Andrew Kourkoutis defines a perfect digital invariant as an integer
      * containing N digits, where the sum of the Nth powers of the digits
      * is equal to the integer itself.
      *
      * In general...
      *
      *    H^N +  ... + I^N  +   J^N  +   K^N = 
      * (H*10^N-1)...(I*10^2)+(J*10^1)+(K*10^0)
      *
      * IF N = 3 THEN
      *
      * I^3 + J^3 + K^3 = I*10^2 + J*10^1 + K*10^0
      *
      * For example...    371 is one such number
      *
      * 3^3 + 7^3 + 1^3 = 371
      *  27 + 343 +   1 = 371
      *
       data division.
       working-storage section.
      *
       01 test-number                 pic 9(18).
       01 candidate-number            pic 9(18).
       01 number-length               pic 99     value 6.
       01 digit.
          02 d                        pic 99
                occurs 3 to 18 times depending on number-length.
       01 loop                        pic 99.
       01 total                       pic 9(18)  value zero.
       01 min-number                  pic 9(18).
       01 max-number                  pic 9(18).
      *
       procedure division.
       main-para.
	   display "Perfect Digital Invariant"
           compute min-number = 10 ** (number-length-1)
           compute max-number = 10 ** number-length - 1
           perform loop-test-numbers until test-number is equal to max-number
           stop run.
      *
       loop-test-numbers.
           move min-number to test-number
           move zero to total
           move test-number to candidate-number
	   move number-length to loop
           perform digit-loop until loop is equal to zero
           move number-length to loop
           perform sum-loop until loop is equal to zero
           if total is equal to test-number then
              display test-number
           end-if
           add 1 to test-number.
      *
       digit-loop.
           compute d(loop) = function integer(candidate-number / 10 ** (loop - 1))
           compute candidate-number = candidate-number - d(loop) * 10 ** (loop - 1)
           subtract 1 from loop.
      *
       sum-loop.
           compute total = total + d(loop) ** loop
           subtract 1 from loop.
      *
