{-
Problem 4

Largest palindrome product

A palindromic number reads the same both ways. The largest palindrome made from the product of two 2-digit numbers is 9009 = 91 × 99.

Find the largest palindrome made from the product of two 3-digit numbers.

Solution:

one can do "Brute-force"

Answer:
906609
-}

module E4 where

import System.CPUTime
picoSecond = 1e12 :: Double



isPalindrome :: Int -> Bool
isPalindrome x =    xd == reverse xd
    where xd = show x

--allCombs = [ x*y | x<-[1..99],y<-[1..99]] :: [Int]
allCombs = [ x*y | x<-[100..999],y<-[100..999]] :: [Int]

main = do 
    putStrLn "Brute force:"
    start <- getCPUTime
    print $ foldr (\x y -> if (isPalindrome x) && x>y then x else y) 0 allCombs
    end <- getCPUTime
    print $ show (fromIntegral (end-start)/picoSecond) ++ " [sec]"
