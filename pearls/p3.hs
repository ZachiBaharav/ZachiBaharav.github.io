{-

Pearl 3: Improving on Saddleback search

Definitions:
Given function f(x,y) -> z , where x,y,z are Natural numbers.
f( , ) is strictly increasing in both it's arguments.

Problem:
Write the function invert which returns all the pairs (x,y) such that
f(x,y)=z

Solution:

Two methods: One that 'kind off' goes along a 'line' from top-left to bottom right, walking on the 'iso-line'.
The second is based on binary search, with the right limits, on different rows/columns

-}
module P3 where

-- Test function
f (x,y) = x+2*y
--f (x,y) = 3*x+27*y+y^2
--f (x,y) = x^2+y^2+x+y


-- Brute force
-- Requires (z+1)^2 evaluations of f
invert1 f z = [(x,y) | x<-[0..z],y<-[0..z],f (x,y) == z]

-- Saddleback search
-- Going from the top left to the bottom right, but moving to the right
-- in smart way, so NOT searching the whole triangle. More like searching
-- along a line.
invert2 f z = find2 (0,m) f z n
				where
					-- determine boundaries of 'box' to search"
					-- m on the y-axis, n on the x
					-- we will search in the (0,0) (m,n) box
					m = bsearch (\y->f(0,y)) (-1,z+1) z
					n = bsearch (\x->f(x,0)) (-1,z+1) z 

find2 (u,v) f z n
	| u > n || v < 0	=	[]							-- if we are out of the box: Stop
	| z'<  z			= 	find2 (u+1,v) f z n			-- we started from the TOP on the y-axis in every column
														-- so if we stepped down on the column and didn't find it,
														-- move one column to the right
	| z'== z			= 	(u,v) : find2 (u+1,v-1) f z n-- We found one!! go to the right
	| z'>  z 			=	find2 (u,v-1) f z n			-- Keep going down this column. we are still too large.
	where
		z' = f(u,v)



-- regular binary search
bsearch g (a,b) z
	| a+1 == b 		= a 				-- no more 'segment' left
	| g m <= z		= bsearch g (m,b) z -- look at the top segment
	| otherwise 	= bsearch g (a,m) z -- look at the bottom segment
	where
		m = (a + b) `div` 2


-- Binary search-2D, full swing
invert3 f z = 	find3 (0,m) (n,0) f z
				where
					m = bsearch (\y->f(0,y)) (-1,z+1) z
					n = bsearch (\x->f(x,0)) (-1,z+1) z 

find3 (u,v) (r,s) f z
	| u > r || v < s 	= []	-- out of bounderies
	| v-s <= r-u 		= rfind (bsearch (\x->f(x,q)) (u-1,r+1) z)	-- Rows are longer than columns: search along row
	| otherwise 		= cfind (bsearch (\y->f(p,y)) (s-1,v+1) z)	-- Column search
	where
		p 		= (u+r) `div` 2
		q		= (v+s) `div` 2
		rfind p = (if f (p,q) == z 	then (p,q): find3 (u,v) (p-1,q+1) f z 	-- Top-Left Rectangle
									else find3 (u,v) (p,q+1) f z ) ++
					find3 (p+1,q-1) (r,s) f z								-- Bottom-Right rectangle

		cfind q = find3 (u,v) (p-1,q+1) f z ++								-- Top-Left
				  (if f (p,q) == z 	then (p,q): find3 (p+1,q-1) (r,s) f z	-- Bottom-Right
									else find3 (p+1,q) (r,s) f z ) 
					


-- Main

main = do
	putStr "Brute force       : " 
	print $ invert1 f 18
	putStr "Saddleback search : " 
	print $ invert2 f 18
	putStr "Binary 2D search  : " 
	print $ invert3 f 18
