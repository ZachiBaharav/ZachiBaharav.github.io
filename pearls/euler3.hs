{-
Problem 3

Largest prime factor
The prime factors of 13195 are 5, 7, 13 and 29.

What is the largest prime factor of the number 600851475143 ?


Solution:

one can do "Brute-force"

Answer:
6857
-}

module E3 where

import System.CPUTime
picoSecond = 1e12 :: Double

--m = 13195 :: Integer
m = 600851475143 :: Integer

isPrime     :: Integer -> Bool
isPrime m   = isPrime' m 2
                where
                    isPrime' :: Integer -> Integer -> Bool
                    isPrime' m i
                        |   i*i > m         = True
                        |   m `rem` i == 0  = False
                        |   otherwise       = isPrime' m (i+1)

factors     :: Integer -> [Integer]
factors m   = factors' m 2
                where
                    factors'    :: Integer -> Integer -> [Integer]
                    factors' m i 
                        |   m==1            =   []
                        |   m `rem` i == 0  =   i : factors' (m `div` i) 2
                        |   otherwise       =   factors' m  (i+1)


main = do 
    putStrLn "Brute force:"
    start <- getCPUTime
    print $ head $ reverse $ filter isPrime $ factors m
    end <- getCPUTime
    print $ show (fromIntegral (end-start)/picoSecond) ++ " [sec]"
