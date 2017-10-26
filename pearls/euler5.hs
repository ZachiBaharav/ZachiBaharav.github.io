{-

Problem 5

Smallest multiple

2520 is the smallest number that can be divided by each of the numbers from 1 to 10 without any remainder.
What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?

Solution:

For 1..10 : 
Factors of all the elements:
1 - 1
2 - 2
3 - 3
4 - 2,2
5 - 5
6 - 2,3
7 - 7
8 - 2,2,2
9 - 3,3
10 - 2,5

The union of all this (which means, we keep the longest '2's etc):
1,2,3,2,5,7,2,3 . and their product: 2520

for 1..20:
similarily, we'll do it programmatically.
Answer:
232792560
-}

module E5 where

import Data.List (nub, intersect,(\\))
import System.CPUTime
picoSecond = 1e12 :: Double

--m = 10     -- example: Answer =2520
m = 20

-- from Euler3
factors     :: Integer -> [Integer]
factors m   = factors' m 2
                where
                    factors'    :: Integer -> Integer -> [Integer]
                    factors' m i 
                        |   m==1            =   []
                        |   m `rem` i == 0  =   i : factors' (m `div` i) 2
                        |   otherwise       =   factors' m  (i+1)

-- takes two lists, and returns a union where each one of the lists is a subset of the union
-- Few things to note:
-- the lists are ordered (this is how factors does them)
-- Duplicates are allowed
-- We can do in different ways, but it seems the simplest is:
listCountUnion          :: [Integer] -> [Integer] -> [Integer]
listCountUnion  a b     = c' ++ (a \\ b) ++ (b \\ a) 
                            where
                                c = (nub a) `intersect` b
                                c' = concat $ map (\x-> replicate (lab a b x) x)  c
                                lab a b x = min  (length $filter (==x) a) (length $filter (==x) b)


main = do 
    putStrLn "Brute force:"
    start <- getCPUTime
    print $  product $ foldr1 listCountUnion $ map factors [1..m]
    end <- getCPUTime
    print $ show (fromIntegral (end-start)/picoSecond) ++ " [sec]"
