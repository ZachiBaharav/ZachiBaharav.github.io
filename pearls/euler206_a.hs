{-

Comment about this version of solution:
This is the same problem (Euler206), but this time we will use
'proper' design pattern for Depth-First Search (DFS).

In the  below, i am following pretty closely the 
pattern discussed in "Pearls of Functional Algorithm Design"
for the Rush-Hour problem.

===
*** Problem 206: Concealed Square
Find the unique positive integer whose square has the form 1_2_3_4_5_6_7_8_9_0,
where each “_” is a single digit.

*** Solution
Searching 'all possible numbers' using Depth First Search, and using
'pickDigit' to check compliance with the goal along the way.

===

Few design decisions:
1. Numbers are stored as list of digits, namely  [Int]. Main reason is to 
deal with leading zeros in intermediate stages (think about the case when
in intermediate stage your candidate number is 00089, and the final result is
123400089) !
   1.a. 'Don't care' digits are '-1'
2. Depth First Search strategy: we chose that for exercsie.


-}

module Main where

import Data.List (findIndices)
import Data.Maybe (isJust, fromJust)
import Debug.Trace

-- General design pattern for a puzzle solver, with 
-- DFS (Depth First Search) and BFS (Breadth First Search)

type Move  = Int 		-- In our case: Adding a digit
type State = [Int]		-- In our case: Current number candidate. Collection of digits

moves  :: State -> [Move]		 -- All the new moves we can do from a state
								 -- We'll define specific for our case below.

move   :: State -> Move -> State -- What happens when we apply a move
move ds d = d:ds 				 -- We add the new digit to the front of the digits

solved :: State -> Bool          -- Are we done yet?
                                 -- We'll define specific for our case below.

solve  :: State -> Maybe [Move]  -- Starting from state 'q', returns list of moves to success
								 -- or Nothing if none
solve = dfsolve 	   -- or bfsolve

-- For the dfs/bfs, we will introduce these synonyms

type Path 		= ([Move],State)  -- Sequence of moves made from given state,
 								  -- and the resulting end state from these.
 								  -- Usually it will be from the starting of game.
type Frontier 	= [Path]		  -- List of Paths to be explored


bfsearch :: [State] -> Frontier -> Maybe [Move]
                                -- [State]  - All the states we visited alread
                                -- Frontier - Paths to be explored
                                -- Maybe [Move] - Solution (or not)

bfsearch qs [] = Nothing		-- If we do not have any frontiers to explore, we are done.
bfsearch qs (p@(ms,q):ps)
	| solved q 		= Just ms 			-- Eureka: Solved!
	| q `elem` qs	= bfsearch qs ps 	-- No need to consider q (or this path). 
										-- It was already analyzed
	| otherwise 	= bfsearch (q:qs) (ps++ succs p)
										-- q is not the solution, and we didn't see it before,
										-- So, we need to explore it:
										-- We add the state to the list of ones we got to,
										-- and we add the extended search paths to
										-- frontier.
										-- in bfs, the new state are added at the end (=queue)

succs        :: Path -> [Path]
succs (ms,q) = [ (ms++[m],move q m) | m <- moves q] 


-- DFS is like BFS, but with one change:
-- How do we add the new paths to explore?
-- Here, we add these at the front (=stack)
dfsearch :: [State] -> Frontier -> Maybe [Move]
dfsearch qs [] = Nothing		
dfsearch qs (p@(ms,q):ps)
	| solved q 		= trace ("In dfsearch. Found it! \n" ++ show (ms) )$ 
						Just ms 			
	| q `elem` qs	= dfsearch qs ps 	
	| otherwise 	= trace ("In dfsearch:\n" ++ show (ms) )$ 
						dfsearch (q:qs) (succs p ++ ps )  -- the change!!


bfsolve :: State -> Maybe [Move]
bfsolve q = bfsearch [q] []

dfsolve :: State -> Maybe [Move]
dfsolve q = dfsearch [q] f0
	where f0 = [([m],[m]) | m<-[0..9]] :: Frontier


main = do
	-- print $ findNumbers [] [] 
	putStrLn $ if (isJust sol)
				then show $ reverse (fromJust sol)
				else "No solution found"
	where	
		sol = solve [] 

-- testing cases
dontCare = -1 :: Int
givenDigits = ([1..9]++[0]) ||| (replicate 9 dontCare) :: [Int]
-- see below for |||.
-- Some testing cases later on:
--givenDigits = [1,dontCare,4] :: [Int]
--givenDigits = [1,6,dontCare,dontCare] :: [Int]
--givenDigits = [1,dontCare,dontCare] :: [Int]
--givenDigits = [dontCare,dontCare,4] :: [Int]


-- Three 'bridging' fucntions from general terminology to our game

-- moves ds = [0..9]			 -- In our case: If we were doing brute force
moves ds = pickNextMove ds       -- In our case: Choosing only some... This is GAME specific


pickNextMove :: State -> [Move]
pickNextMove ds = pickDigits ds [0..9]  -- Choosing only some... This is GAME specific

solved = completeNum			 -- Is the number complete? 

-- Just a little bit more about these functions

pickDigits :: [Int] -> [Int] -> [Int]
pickDigits ds tryds = findIndices (\x->x) tf
	where
		tf = map (tryDigit ds) tryds

completeNum :: [Int] -> Bool
completeNum ds = compareDigits ds2 givenDigits (length givenDigits)
	where
		ds2 =  num2Digits $ (digits2Num ds)^2 

-- And now for the detailed stuff...
-- utility functions
digits2Num :: [Int] -> Integer
digits2Num ds = foldl (\x y -> 10*x+(fromIntegral y)) 0 ds

num2Digits :: Integer -> [Int]
num2Digits 0 = []
num2Digits n = num2Digits (n `div` 10) ++ [(fromIntegral n) `mod` 10]

compareDigits :: [Int] -> [Int] -> Int -> Bool
compareDigits ds ys l = (length ds >=l) && (length ys >=l) &&
						( compareDigitsR (take l $ reverse $ ds)  (take l $ reverse $ ys) )

compareDigitsR :: [Int]->[Int]->Bool
compareDigitsR ds ys = and $ zipWith (f) ds ys
		where
			f d y = ( (d==dontCare) || (y==dontCare) || (d==y) )

-- Game functions

tryDigit :: [Int] -> Int -> Bool
tryDigit ds d = compareDigits givenDigits n2 (1+length ds)
	where
		n2 = n2Digits (d:ds)

-- The tricky thing here is to deal with leading zeros!
-- Let's say the number we are looking for is
--  345000089 . After the first digits 89, unless we allow
-- leeading zeros, we will be lost/stuck.
n2Digits :: [Int] -> [Int]
n2Digits ds = n2d
	where
		n' = digits2Num (ds)
		newNum' = num2Digits $ n'*n'
		-- taking care of leading zeros
		pad = (length ds)+1 - (length newNum')
		n2d =  (replicate  pad 0) ++ newNum'

-- a fun way to interleave two lists. works also for infinte ones.
infixr 5 |||
(|||) :: [a] -> [a] -> [a]
[]     ||| ys = ys
(x:xs) ||| ys = x : ys ||| xs


--
test0 = tryDigit [3,0] 0
test1 = tryDigit [0,0,3,0] 0
test2 = pickDigits [3,0] [0..9]
q = [3,0] :: [Int]





