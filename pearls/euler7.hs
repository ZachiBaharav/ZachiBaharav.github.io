{-
Problem 7

10001st prime

By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see that the 6th prime is 13.

What is the 10 001st prime number?


Answer:
104743
-}

module E7 where

--m = 6
m = 10001

a = [1..]


main = do 
    print $   (filter isPrime a) !! m


isPrime     :: Integer -> Bool
isPrime m   = isPrime' m 2
                where
                    isPrime' :: Integer -> Integer -> Bool
                    isPrime' m i
                        |   i*i > m         = True
                        |   m `rem` i == 0  = False
                        |   otherwise       = isPrime' m (i+1)
