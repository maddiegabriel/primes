*> -*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
*> file:   primes3.cob
*> goal:   identifies prime numbers from a dynamic input file OR std input.
*>         prints result to a dynamic output file OR to std output.
*>-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

identification division.
program-id. primes3.

*> specify file control variables (dynamic input/output files)
environment division.
input-output section.
file-control.
select in-file assign to dynamic in-fname
    organization is line sequential.
select out-file assign to dynamic out-fname
    organization is line sequential.
select std-in assign to keyboard.

*> specify file characteristics and declare all variables
data division.
file section.
fd out-file.
    01 out-line pic x(100).

*> declare all variables/types
working-storage section.
77 input-choice pic 9 value 1.
77 counter pic s9(9) value 0.
77 nums-size pic s9(9) value 0.
77 in-fname pic x(30).
77 out-fname pic x(30).
77 eof-switch pic 9 value 1.
77 curr-num pic s9(9) value 1.
77 print-num pic ZZZZZZ9.
77 i pic s9(9) usage is computational.
77 result pic 99.
77 rem pic s9(9) usage is computational.
77 flag pic 9 value 0.
77 is-prime pic s9(2) value 0.
01 array.
    02 nums pic s9(9) occurs 100 times.
01 in-card.
    02 in-num pic z(9).
    02 filler pic x(71).
01 title-line.
    02 filler pic x(6) value spaces.
    02 filler pic x(20) value 'PRIME NUMBER RESULTS'.
01 under-line.
    02 filler pic x(32) value ' -------------------------------'.
01 not-a-prime-line.
    02 filler pic x value space.
    02 out-num-not pic z(8)9.
    02 filler pic x(15) value ' IS NOT A PRIME'.
01 prime-line.
    02 filler pic x value space.
    02 out-num-prime pic z(8)9.
    02 filler pic x(11) value ' IS A PRIME'.
01 error-mess.
    02 filler pic x value space.
    02 out-num-error pic z(8)9.
    02 filler pic x(14) value ' ILLEGAL INPUT'.

procedure division.
    introduction.
        display ' '.
        display '------------------------------------'.
        display '  WELCOME TO MY PRIMES CALCULATOR!'.
        display '   Built with love for CIS*3190'.
        display '   By: Maddie Gabriel (0927580)'.
        display '------------------------------------'.
        display ' '.

    *>  ask user whice input/output method they want
    choose-input.
        open input std-in.
        display "TYPE 1 to enter numbers through the keyboard (standard input)".
        display "TYPE 2 to enter numbers from a file".
        display ' '.
        display "My Choice: " no advancing.
        accept input-choice.
        display ' '.

    *> get number input from user for each choice
    number-input.

        if input-choice = 1

            display ' '
            display 'Please enter each integer (> 1) one at a time.'
            display 'To STOP and see your results, enter 0 or a blank line.'
            display ' '
            
            *> add each number to array nums
            perform until curr-num = 0
                display 'Enter an integer: '  no advancing
                accept nums(nums-size)
                move nums(nums-size) to curr-num
                add 1 to nums-size
            end-perform

            *> print results header
            display ' '
            display ' '
            display '   PRIME NUMBER RESULTS'
            display ' --------------------------'

            *> loop through nums array and check each one
            perform until counter = (nums-size - 1)
                move nums(counter) to curr-num
                if curr-num is not = 0
                    move nums(counter) to print-num
                    *> call paragraph to check if this current number is prime
                    perform prime-check
                    *> display proper result line to std in
                    if is-prime = 0
                        display print-num ' IS NOT A PRIME'
                    else if is-prime = 1
                        display print-num ' IS A PRIME'
                    else
                        display print-num ' ILLEGAL INPUT'
                    end-if
                end-if
                add 1 to counter
            end-perform
            display ' '
        else

            *> get user input for the input/output file names
            display 'NOTE: Please include extension in filenames (example: input.txt)'
            display ' '
            display "Enter input filename: " no advancing
            accept in-fname
            display ' '
            display "Enter output filename: " no advancing
            accept out-fname
            display ' '
            
            *> open files and write header to output file!
            open input in-file, output out-file
            write out-line from title-line after advancing 0 lines
            write out-line from under-line after advancing 1 line
            
            *> call paragraph to read input file and calculate if prime
            perform file-read
                until eof-switch = 0
            
            display '-----------------------------------------'
            display '           DONE - THANK YOU!'
            display '   Please see your results in ' out-fname
            display '-----------------------------------------'
            display ' '
            close in-file, out-file
        end-if.

stop run.

*> paragraph to read each number in the file and write the result
file-read.
    *> start reading the input file
    *> at end of file, stop looping by setting the variable eof-switch to 0
    read in-file into in-card
        at end move zero to eof-switch.
    
    if eof-switch is not equal to zero
    
        *> move current num from input file (in-n) to variable n
        move in-num to curr-num
        
        *> call paragraph to check if this current number is prime
        perform prime-check
        
        *> write proper result line to output file
        if is-prime = 0
            write out-line from not-a-prime-line after advancing 1 line
        else if is-prime = 1
            write out-line from prime-line after advancing 1 line
        else
            write out-line from error-mess after advancing 1 line
        end-if
        
    end-if.

*> paragraph to check if an indiviudal number is prime
prime-check.
    *> reset  variables
    move 0 to flag
    move 0 to result
    move 2 to rem
    move 0 to is-prime
    move in-num to out-num-error
    move in-num to out-num-not
    move in-num to out-num-prime
        
    if curr-num <= 1
        *> num is 1 or less (invalid input)
        move in-num to out-num-error
        move 2 to is-prime
    else
        if curr-num >= 4
            *> number is valid but 4 or greater, so check if it's prime!
            *> divide num by every number i = [2,num]
            perform varying i from 2 by 1 until i >= curr-num
                divide curr-num by i giving result remainder rem
                *> if the remainder is 0, it is divisible so it's not prime!
                *> mark the flag and stop looping
                if rem = 0
                    move 1 to flag
                    move curr-num to i
                end-if
            end-perform
    
            *> set is-prime value and fill appropriate out-line variable
            if flag = 0
                move in-num to out-num-not
                move 1 to is-prime
            else
                move in-num to out-num-prime
                move 0 to is-prime
            end-if
        else
            *> number is valid but less than 4, so it is prime!
            move in-num to out-num-prime
            move 1 to is-prime
        end-if
    end-if.
