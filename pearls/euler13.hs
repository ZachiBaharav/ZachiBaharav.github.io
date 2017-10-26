{-
Large sum

Problem 13
Work out the first ten digits of the sum of the following 
one-hundred 50-digit numbers.

Solution:
Brute force.

-}

-- answer: 5537376230 

module E13 where 


strToNums :: String ->[Integer]
strToNums [] = []
strToNums s = [read (take 50 s) :: Integer ] ++ strToNums (drop 50 s)


main = do
    strs <- readFile "euler13.txt" 
    let nums = map read (lines strs ) :: [Integer]
    print $ take 10 $ show (sum nums)

