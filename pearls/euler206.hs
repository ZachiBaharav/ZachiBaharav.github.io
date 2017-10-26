{-
Concealed Square
Problem 206
Find the unique positive integer whose square has the form 1_2_3_4_5_6_7_8_9_0,
where each “_” is a single digit.

Solution
Depth First Search along possible routes.

Few design decisions:
1. numbers are stored as [Int]. Otherwise, there is problem in converting
back-and-from digits to numbers when you have leading zeros!
2. Depth first search: Wanted as an exercise.
  The trick is in findNumbers, which keeps on looking through all available
  new leaves.

-}

module Main where

import Data.List (findIndices)

digits2Num :: [Int] -> Integer
digits2Num ds = foldl (\x y -> 10*x+(fromIntegral y)) 0 ds

num2Digits :: Integer -> [Int]
num2Digits 0 = []
num2Digits n = num2Digits (n `div` 10) ++ [(fromIntegral n) `mod` 10]

dontCare = -1 :: Int
givenDigits = ([1..9]++[0]) ||| (replicate 9 dontCare) :: [Int]
-- see below for |||.
-- Some testing cases later on:
--givenDigits = [1,dontCare,4] :: [Int]
--givenDigits = [1,dontCare,dontCare] :: [Int]
--givenDigits = [dontCare,dontCare,4] :: [Int]

compareDigits :: [Int]->[Int]->Int->Bool
compareDigits ds ys l = compareDigitsR (take l $ reverse $ ds)  (take l $ reverse $ ys)

compareDigitsR :: [Int]->[Int]->Bool
compareDigitsR [] _  = True
compareDigitsR _  [] = True
compareDigitsR (d:ds) (y:ys) = ( (d==dontCare) || (y==dontCare) || (d==y) )
				&& compareDigitsR ds ys

completeNum :: [Int] -> Bool
completeNum ds = (length n2 == length givenDigits) &&
				(compareDigits n2 givenDigits (length givenDigits) )
	where
		n2 = n2Digits ds

tryDigit :: [Int] -> Int -> Bool
tryDigit ds d = (length n2 <= length givenDigits) &&
				( compareDigits givenDigits n2 (1+length ds) )
	where
		n2 = n2Digits (d:ds)

n2Digits :: [Int] -> [Int]
n2Digits ds = n2d
	where
		n' = digits2Num (ds)
		newNum' = num2Digits $ n'*n'
		-- taking care of leading zeros
		pad = (length ds)+1 - (length newNum')
		n2d =  (replicate  pad 0) ++ newNum'


pickDigits :: [Int] -> [Int] -> [Int]
pickDigits ds tryds = findIndices (\x->x) tf
	where
		tf = map (tryDigit ds) tryds

candidateNums :: [Int] -> [Int] -> [[Int]]
candidateNums ds newds = map (\x -> x:ds) newds

addDigit :: [Int] -> [[Int]]
addDigit ds  
	| null newds	= []
	| otherwise 	= candidateNums ds newds
	where
		newds = pickDigits ds [0..9]

addDigits :: [Int] -> [[Int]]
addDigits ds = concat $ map addDigit (addDigit ds)

findNumbers :: [[Int]]->[Int] -> [[Int]]
findNumbers dss ds
	| completeNum ds 	= ds:dss 
	| otherwise 		= if null ds' then dss
						  else concat $ map (\x -> findNumbers dss x) ds'
	where
		ds' = -- trace (show ds) $ 
				addDigit ds

main = do
	print $ findNumbers [] [] 

--
test0 = tryDigit [3,0] 0
test1 = tryDigit [0,0,3,0] 0
test2 = pickDigits [3,0] [0..9]
q = [3,0] :: [Int]
test3 = candidateNums q  (pickDigits q [0..9])
test4 = addDigit [0,0,3,0]

-- a fun way to interleave two lists. works also for infinte ones.
infixr 5 |||
(|||) :: [a] -> [a] -> [a]
[]     ||| ys = ys
(x:xs) ||| ys = x : ys ||| xs

