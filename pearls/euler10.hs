
{-
Summation of primes
Problem 10
Published on 08 February 2002 at 06:00 pm [Server Time]
The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.

Find the sum of all the primes below two million.

Solution
Brute force.
Answer: 142913828922
-}

module Euler10 where

upTo = 2000000 :: Int
--upTo = 10 :: Int

main = do
	print $ sum $ takeWhile (<upTo) primeList


primeList :: [Int]
primeList = filter isPrime [2..] 

isPrime :: Int -> Bool
isPrime n =  not $ or $ map (\x-> (n `rem` x)==0) [2..n2]
	where 
		n2 = round $ sqrt (fromIntegral n) 


