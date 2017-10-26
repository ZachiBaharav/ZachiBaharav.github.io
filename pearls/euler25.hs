{-
1000-digit Fibonacci number
Problem 25
The Fibonacci sequence is defined by the recurrence relation:

Fn = Fn−1 + Fn−2, where F1 = 1 and F2 = 1.
Hence the first 12 terms will be:

F1 = 1
F2 = 1
F3 = 2
F4 = 3
F5 = 5
F6 = 8
F7 = 13
F8 = 21
F9 = 34
F10 = 55
F11 = 89
F12 = 144
The 12th term, F12, is the first term to contain three digits.

What is the index of the first term in the Fibonacci sequence to contain 1000 digits?

Solution:
"Brute-force"
Score = 4782

-}

module E25 where

numDigits = 1000 :: Int

main = do 
    putStrLn $ "Index is:" ++ (show $ fibNDigits 1 1 2 numDigits)

fibNDigits ::   Integer ->      -- f0: fib n-1
                Integer ->      -- f1: fib n
                Int ->          -- n : n  (index)
                Int ->          -- l0: number of digits needed to terminate
                Int             -- Final index

fibNDigits f0 f1 n l0 = if ((length $ show f2) == l0 ) 
                            then (n+1)
                            else fibNDigits f1 f2 (n+1) l0
                        where
                            f2 = f0+f1


