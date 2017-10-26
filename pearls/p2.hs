{-

Pearl 2: Surpassing

Definitions:
A surpasser of an element of an array (list) is a greater element to the right.
x[j] is a surpasser of x[i] if i<j and x[i]<x[j].
The surpasser count of an element is the number of its surpassers.

Problem:
Compute the maximum surpasser count of an array of length n>1 in an O(n log n) algorithm.

Solution:

Note 1: In the imperative programming, using iterative+binary search.
We will use Divide and Conquer 

Note 2: Cannot do better than O(n log n), b/c this is equivalen to sorting (you will see: 
the table we create)
-}
module P2 where

-- Test word is:         GENERATION   
-- Surpasser count is:   5625140100  
-- So the maximum isL    6
testX = "GENERATION" :: [Char]

-- Maximum Surpasser Count
-- Brute force approach
-- Requires O(N^2) steps
-- Goes on shrinking list of length n: n+(n-1)+(n-2)+.. = n^2/2
msc 		:: 	Ord a => [a] -> Int
msc xs		=	maximum [scount z zs | z:zs <- tails xs]
scount x xs	= 	length (filter (x<) xs)

-- We'll redfine tails, to make sure empty returns empty
tails 			:: [a] -> [[a]]
tails []		= []
tails (x:xs)	= (x:xs):tails xs

-- Divide and Conquer
-- The trick is to find some 'data' that we can join together
-- easily. Then we can divide, and then join.
-- There are a few tricks to get to this 'join':
-- 1. Define data structure that contains enough information. Then you can join them.
--    This data structure is the table, that contains not only the Maximum Surpasser from
--    each divide, but all the counts!
-- 2. If we keep the Table in sorted order, the join can be done easily! (in Linear time)

table [x]		=	[(x,0)]		-- one element
table xs		=	join' (m-n) (table ys)  (table zs)
					where
						m = length xs
						n = m `div` 2
						(ys,zs) = splitAt n xs

join' 0 txs []	= txs
join' n []  tys	= tys
join' n txs@((x,c):txs') tys@((y,d):tys')
		| x<y 	= (x,c+n) : join' n txs' tys	
		| x>=y 	= (y,d) : join' (n-1) txs tys'
-- the lists are ordered within themselves.
-- so if x<y ==> All the rest (and there's length n of tys) are also
-- greater than x ==> we should add this count!
-- if x>y, we just put y.
-- if x==y, we just want to make sure we continue in reduced manner, so we put (y,d), remmove
-- it from the next join, and then move on to continue.

-- Main

main = do
	putStr "Brute force       : " 
	print $ msc testX
	putStr "Divide and Conquer: " 
	print $ maximum $ map (snd) (table testX)
