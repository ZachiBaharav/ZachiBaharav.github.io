{-
Lattice Paths

Problem 15
Starting in the top left corner of a 2×2 grid, and only being able to move to the right and down, 
there are exactly 6 routes to the bottom right corner.

How many such routes are there through a 20×20 grid?
Solution:
Math.

We will need 40 steps. 20 of them are right, and 20 are down.
We just need to choose 20, and the order doesn't matter within these.

-}

-- answer: 137846528820

module E15 where 

-- ans = 40 choose 20
ans = (product [21,22..40]) `div` (product [1..20]) :: Integer


main = do
    print $ ans

