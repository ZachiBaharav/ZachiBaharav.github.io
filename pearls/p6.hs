{-

Pearl 6: Making a century

Problem:
Given the digits 1..9, list all the ways the operations + and x can be inserted into 
the sequence so as to make the total sum 100.
For example:
100 = 12 + 34 + 5x6 +7 + 8 + 9
100 = 1 + 2x3 + 4 + 5 + 67 + 8 + 9

No parthneses, normal order of operations.

Solution:
This is a brute-force problem, but the goal of the pearl is to establish some
general formulation of 'Brute-Force' search, and improve on it with 'little'
assumptions.

Two main things for brute-force methods:
1. Generating the 'values' while generating the 'candidates', can produce savings (Especially
if generations of candidates can be 'sequential'). 
2. Reduce 'good' critertia to 'ok', it can help trim the generation of all sequences.

-}

-- You CAN skip all the general formulation description and go to ======

-- General formulation:
-- candidates	:: Data -> [Candidate]
-- value		:: Candidate -> Value
-- good 		:: Value -> Bool

-- Brute force search over all candidates
-- solutions 	:: Data -> [Candidate]
-- solutions	=  filter (good.value).candidates	

-- Now, using a few assumptions we can modify thigns:

-- Assumption 1: This will allow us to fuse operations.
--
-- Data is a list of values, [Datum], and candidates takes the form
-- candidates :: [Datum] -> [Candidate]
-- candidates =  foldr extend []
-- where
-- extend :: Datum -> [Candidate] -> [Candidate]
--
--
-- Assumption 2: This will allow us to extend search only as needed.
-- 
--      2.a There is a predicate 'ok', such that every good value is necesserialy 'ok'
--      2.b candidates with 'ok' values are the extension of candidates with 'ok' value.
--
-- Assumption 3: This will save us computing 'value' of candidate
--
--   map value.extend x = modify x.map value
--  The values of an extended set can be computed from the values of which the extension was
--  built from.
--
--  We then introduce a few operations on 'fork', 'cross', and relates these
-- to zip and unzip.
--   fork (f,g) x  		=  (f x, g x)
--   cross (f,g) (x,y) 	= (f x, g y)
--
-- and we get:

-- solutions 	:: 	Data -> [Candidate]
-- solutions 	= 	map fst.filter (good.snd).foldr expand []

-- expand x 	= 	filter (ok.snd).zip.cross (extend x, modify x).unzip

module P6 where

import Data.List (intercalate)

-- ================================================



-- So all the above was general design-framework.
-- now, to the specific problem (creating 100)

-- Expression 	= sum of terms
-- Term 		= products of factors
-- Factor 		= sequence of digits

							-- Candidate solution are expressions built from + and x
							-- Remember: No parnthesis etc, and simple prioity.
type Expression	= [Term]    -- Each expression is the sum of Terms
type Term 		= [Factor]	-- Each term is the product of factors
type Factor 	= [Digit]	-- Each factor is list of digits
type Digit 		= Int	


-- Just computing the value:
valExpr		::	Expression -> Int
valExpr	 	=	sum.map valTerm 
valTerm  	= product.map valFact 
valFact  	= foldl (\n d -> 10*n+d)  0


good 	:: 	Int -> Bool
good v 	= 	(v==100)

ok		::	Int -> Bool
ok v 	= 	(v<=100)

-- Creating all possible expressions
-- They mention there is a function 'partitions', but never spell it out.
-- so here it is, even though we will NOT use it!
partitions :: [Digit] -> [[[Digit]]]
partitions [] = [[]]
partitions (x:xs) = [[x]:p | p <- partitions xs]				-- x is a prt of it's own
                 ++ [(x:ys):yss | (ys:yss) <- partitions xs]	-- x is with the next ones

-- A difffernet way
expressions xs = foldr extend [] xs

extend 					::	Digit -> [Expression] -> [Expression]
extend x [] 			=	[[[[x]]]]
extend x es 			=	concatMap (glue x) es

-- glue: Each new digit can be added in one of 3-different ways to the expression:
-- as a digit; as factor (multiplication); or as a term ==> 3 options.
-- BTW, this shows the number of possible expressions is 3^(n-1) 
glue 					::	Digit -> Expression -> [Expression]
glue x ((xs:xss):xsss)	= 	[((x:xs):xss):xsss,   	-- digit
							 ([x]:xs:xss):xsss,		-- factor
							 [[x]]:(xs:xss):xsss]	-- term


-- Main

main = do
	-- putStr "Testing partitions: "
	-- print $ partitions [1..4]

	putStrLn "Brute force : " 
	let res = filter (good.valExpr) $ expressions [1..9]
	--print res 
	prettyPrint res


-- pretty
prettyPrint es 		= 	putStr $ unlines $ map printExpression es 

printExpression 	::  Expression -> String
printExpression e 	= 	"100 = " ++ 
						( intercalate  " + " $ map printTerm e)

printTerm t 		=  	intercalate  "*" $ map printFactor t
printFactor f 		=  	concat $ map show f
