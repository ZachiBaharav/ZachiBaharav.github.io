{-

Pearl 4: Selection problem

Definitions:
X and Y two finite disjoint sets, sorted.

Problem:
Find the k-smallest elemnt in the set (X union Y)

Solution:
You can do it of course in linear time O(K).
But it turns out you can do faster by divide and conquer.
The trick is to do the merge-decisions in the right way.

We then also use arrays rather than lists, to get efficency in time.

<== And this is where it seems the book messed up on indices. Corrected in my code.

I left all the debugging and printing in there, so one can see the equivalence between
Array-indices and Lists.

-}
module P4 where

import Data.Array   (Array, listArray, bounds, (!))
import Debug.Trace

-- Test function(s)
{-
x = [7,14..28]
y = [5,10..35]
k = 4  
-}
y = [7,14..28]
x = [5,10..35]
--k = 4  
k = 7

xa = listArray (0,length x-1) x
ya = listArray (0,length y-1) y

-- Brute force
-- Requires (z+1)^2 evaluations of f
smallest1 			:: 	Ord a => Int -> ([a],[a]) -> a
smallest1 k (xs,ys)  = 	union (xs,ys) !!k

union (xs,[])		= 	xs
union ([],ys)		= 	ys
union (x:xs,y:ys)	|	x < y 	= x : union(xs,y:ys)
					| 	x > y 	= y : union(x:xs,ys)



-- Divide and Conquer
smallest2 k ([],ws)	= ws !! k
smallest2 k (zs,[])	= zs !! k
smallest2 k (zs,ws)	= 
	trace  ("zs=" ++ show zs ++ 
	"  and  ws=" ++ show ws  ++
	" and k=" ++ show k ++ "\n" ++
	" and (p,q)=" ++ show (p,q) ++
	"and (a,b)=" ++ show(a,b) )$
	case (a < b, k <= p+q) of
		(True,True)   -> trace "--> (1)" $ smallest2 k (zs,us)
		(True,False)  -> trace "--> (2)" $ smallest2 (k-p-1) (ys,ws)
		(False,True)  -> trace "--> (3)" $ smallest2 k (xs,ws)
		(False,False) -> trace "--> (4)" $ smallest2 (k-q-1) (zs,vs)
	where
		p 			  = (length zs) `div` 2
		q 			  = (length ws) `div` 2
		(xs,a:ys)	  = splitAt p zs
		(us,b:vs)	  = splitAt q ws


-- Divide and Conquer, using arrays

smallest3 			:: 	Ord a => Show a => Int -> (Array Int a, Array Int a) -> a
smallest3 k (xa,ya)	= 	search k (0,m+1) (0,n+1)
						where
							(0,m) = bounds xa
							(0,n) = bounds ya
							-- The below is actually writing smallest2, but using array notation!
							search k (lx,rx) (ly,ry)
								| lx==rx 	= ya ! ly 	-- x = []
								| ly==ry 	= xa ! lx 	-- y = []
								| otherwise	= 	trace  ("(lx,rx)=" ++ show (lx,rx) ++ 
												"  and  (ly,ry)=" ++ show (ly,ry)  ++
												" and k=" ++ show k ++ "\n" ++
												" and (mx,my)=" ++ show (mx,my) ++
												" and (mx',my')=" ++ show (mx',my') ++
												"and (xa!mx,ya!my)=" ++ show(xa!mx,ya!my) )$
												case ( (xa!mx) < (ya!my), k <= mx'+my') of
												(True,True)   -> trace "--> (1)" $ search k (lx,rx) (ly,my)
												(True,False)  -> trace "--> (2)" $ search (k-mx'-1) (mx+1,rx) (ly,ry)
												(False,True)  -> trace "--> (3)" $ search k (lx,mx) (ly,ry)
												(False,False) -> trace "--> (4)" $ search (k-my'-1) (lx,rx) (my+1,ry)
											where
												mx 			  = (lx + rx) `div` 2 
												my 			  = (ly + ry) `div` 2 
												mx' 		  = (rx - lx) `div` 2 
												my' 		  = (ry - ly) `div` 2 

-- Main

main = do
	print $ "x=" ++ (show x)
	print $ "y=" ++ (show y)
	print $ "union (x,y)=" ++ (show $ union (x,y))
	print $ "Looking for element : " ++ (show k) ++ " (remember, indexing starts from 0)"
	putStr "Brute force                   : " 
	print $ smallest1 k (x,y)
	putStrLn "Divide-and-Conquer (w/ lists) : " 
	print $ smallest2 k (x,y)
	putStr "Divide-and-Conquer (w/ arrays): " 
	print $ smallest3 k (xa,ya)
