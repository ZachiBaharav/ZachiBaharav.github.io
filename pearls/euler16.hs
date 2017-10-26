{-
Power digit sum

Problem 16

2^15 = 32768 and the sum of its digits is 3 + 2 + 7 + 6 + 8 = 26.

What is the sum of the digits of the number 261000?
Solution:
Brute force.

-}

-- answer: 1366

module E16 where 


a = 2^1000 :: Integer
str = show a 
digits = map (\x-> (fromEnum x) - (fromEnum '0')) str

main = do
    print $ sum digits

