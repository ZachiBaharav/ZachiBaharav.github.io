{-
Problem 6

Sum square difference

The sum of the squares of the first ten natural numbers is,

12 + 22 + ... + 102 = 385
The square of the sum of the first ten natural numbers is,

(1 + 2 + ... + 10)2 = 552 = 3025
Hence the difference between the sum of the squares of the first ten natural numbers and the square of the sum is 3025 − 385 = 2640.

Find the difference between the sum of the squares of the first one hundred natural numbers and the square of the sum.
Answer:
25164150
-}

module E6 where

--m = 10
m = 100

a = [1..m]

a1 = sum $ map (^2) a
a2 = (sum a) ^2 

main = do 
    print $  a2-a1
