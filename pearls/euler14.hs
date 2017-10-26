{-
Longest Collatz sequence

Problem 14

The following iterative sequence is defined for the set of positive integers:

n -> n/2 (n is even)
n -> 3n + 1 (n is odd)

Using the rule above and starting with 13, we generate the following sequence:

13 -> 40 -> 20 -> 10 -> 5 -> 16 -> 8 -> 4 -> 2 -> 1
It can be seen that this sequence (starting at 13 and finishing at 1) contains 10 terms. 
Although it has not been proved yet (Collatz Problem), it is thought that all starting numbers finish at 1.

Which starting number, under one million, produces the longest chain?

NOTE: Once the chain starts the terms are allowed to go above one million.
Solution:
Brute force.

-}

-- answer: 837799, length 525

module E14 where 

import Data.List (maximumBy)

numsTest = [1..13]
numsAll = [1..(10^6-1)]

collatzRoute :: Int -> [Int]
collatzRoute n
    | n==1      = [1]
    | odd n     = n : collatzRoute (3*n+1)
    | otherwise = n : collatzRoute   (fromIntegral n `div` 2)



main = do
    let theOnes = zip numsTest (map collatzRoute numsTest)
    let theOne = foldr1  (\x y -> if (length (snd x)) > (length (snd y)) then x else y) theOnes
    print $ "theOne is: " ++ (show theOne)
    print $ "Length: " ++ (show $ length (snd theOne) )
    print $ theOnes

    let theTwos = zip numsAll (map collatzRoute numsAll)
    let theTwo = foldr1  (\x y -> if (length (snd x)) > (length (snd y)) then x else y) theTwos
    print $ "theTwo is: " ++ (show theTwo)
    print $ "Length: " ++ (show $ length (snd theTwo) )
