{-

Pearl 1: Smallest Free Number

Problem:
Assume X is a list of distinct Natural numbers (non-negative integers), in no particular order.
Find the smallest number not in the list.

We will show two algorithms that do it in LINEAR time.
On first sight, it is not clear such exist. After all, sorting a list is no linear time.
But there's a trick.

The key trick: not every number in the range [0..length xs] can be in xs!
Thus, the smallest number NOT in xs is then the smallest number not in
filter (<=n) xs, where n=length xs.

The two programs:
Array based: builds a checklist of the numbers present in filter (<=n) xs.

Divide and conquer: 
express minfree(xs++ys) in terms of minfree xs  and minfree ys.

-}
module P1 where

import Data.Array 	(Array, elems, accumArray)
import Data.List 	(partition)
testX = [8,23,9,0,12,11,1,10,13,7,41,21,5,17,3,19,2,6] :: [Int]

type Nat = Int 	-- Just for ease of reading things here. Not correct!!

-- Brute force approach
-- Requires O(N^2) steps
-- (see implemntation of (\\) )
minfree :: [Nat] -> Nat
minfree xs = head ([0..] \\ xs)

-- where  us (\\) vs    denotes the list of those elements in us remaining after removing
-- any elements in vs.
(\\)	:: Eq a => [a] -> [a] -> [a]
us \\ vs = filter ( `notElem` vs) us

-- Array based method: Build the checklist as an array
-- We will use the 'tricky' function accumArray, which will enable us to do it in linear time.
-- accumArray: Constructs an immutable array from a list of associations. 
-- Unlike array, the same index is allowed to occur multiple times in the list of associations; 
-- an accumulating function is used to combine the values of elements with the same index.

checkList 		:: [Nat] -> Array Nat Bool
checkList xs 	=  accumArray 		--
					(||) 			-- accumlating function. not realy used, as no duplicates!
					False			-- inital entry for each index
					(0,n)			-- (lower/upper) indices of new array
					(zip (filter (<= n) xs) (repeat True) )	-- index-value pairs
					where
						n = length xs
-- Just for fun: Another use of accumArray: Sorting array in LINEAR time, if we know all
-- values lie between (0,n)
-- in our case, we can then look for the first '0' in the resulting array

countList		::  [Nat] -> Array Nat Nat
countList xs	=   accumArray (+) 0 (0,n) ( zip xs (repeat 1))
					where 
						n = length xs

-- Once we have the array, finding the minimum is simple:
-- (elems converts array into a list)
search :: Array Nat Bool -> Nat
search = length.takeWhile id.elems

-- Divide and conquer
minFree xs			=	minFrom 0 (length xs,xs)
minFrom a (n,xs)	|	n==0		= a
					|	m==b-a		= minFrom b (n-m,vs)	-- if us has exactly the number of spots,
															-- and since there's no repetiion, it means
															-- the gap have to be in the OTHER series:
															-- so we continue the search with vs
					|	otherwise	= minFrom a (m,us)		-- if us had the gap ==> keep searching there!
						where
							(us,vs) = partition (<b) xs		-- partitioning to 'two halves'
							b 		= a + 1 + n `div` 2 	-- b is the middle of [a..n]
							m 		= length us

-- Main

main = do
	putStr "Brute force       : " 
	print $ minfree testX 
	putStr "Array method      : " 
	print $ search $ checkList testX 
	putStr "Divide and Conquer: "
	print $ minFree testX 
