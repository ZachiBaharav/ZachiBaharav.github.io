{-
From "Pearls of Functional Algorithm Design", by Richard Bird

Chapter 9.

** Problem: Celebrity Clique.

Set of P people in a Party.
Subset C of P is called "Celebrity Clique" if it's not empty, 
and everyone in the party knows every member of C, but members
C know only each other.

Assumin gthere is such a clique, we need to find it.

We are given the set P (list, not containing duplicates), 
and a binary predicate 'knows'.

** Formaulation of the problem
Set annotation:
C is celebrity-clique of P if
C \in P, C \not empty , and
{ \forevery x \in P, \forevery y \in C ::
            x `knows` y  AND ( y `knows` x ==> x \in C )}

** Solution (Brute force):
    Create all subsequences.
    For each subsequence, check if the condition above holds.

** Solution (in Linear time)
    See the book for full explanation. The point: We know there IS a clique. 
    we just need to find it.
-}

module P9 where 
-- For our party, we will have a set of 6 people.
-- The celebrity clique will be 1 and 3.
ps = [1..6] :: [Int]
aKnowsb :: [(Int,Int)]
aKnowsb =   [ (2,1),(3,1),(4,1),(5,1),(6,1)] ++
            [ (1,3),(2,3),(4,3),(5,3),(6,3)] ++
            [ (2,4),(5,4)]

knows :: Int -> Int -> Bool
knows x y = (x,y) `elem` aKnowsb

--------------------
--  Brute force solution
cclique :: [Int] -> [Int]
cclique ps = head (filter (\x -> isCC x ps) (subseqs ps)) 

-- is this subgroup a celebrity-clique?
isCC :: [Int] -> [Int] -> Bool
isCC cc ps = and [  (x `knows` y)  && 
                ( (not (y `knows` x) ) || ((y `knows` x) && (x `elem` cc)) )
                | x<-ps , y<-cc, x /=y] 

-- All possible subsequences, in descending order of length
-- (longest one first)
subseqs :: [Int] -> [[Int]]
subseqs []      = [[]]
subseqs (x:xs)  = map (x:) (subseqs xs) ++ subseqs xs


--------------------
--  Linear solution

-- just noting. This is the base for the fusion work.
-- Not used here, but used in the derivation.
subseqs' :: [Int] -> [[Int]]
subseqs' xs = foldr add [[]] xs

add :: Int -> [[Int]] -> [[Int]]
add x xss = map (x:) xss ++ xss

cclique' :: [Int] -> [Int]
cclique' ps = foldr op [] ps

-- cs is the current clique. p is the new person
op :: Int -> [Int] -> [Int]
op p cs |   null cs             = [p]   -- We know there's a clique, so if it's 
                                        -- empty so far, p is 'it'.
        |   not (p `knows` c)   = [p]   -- if the new person doesn't know c, 
                                        -- cs is blown off, and we start afresh.
        |   not (c `knows` p)   = cs    -- if we got here, that means that p knows c
                                        -- now, if c doesn't know p, we are good with cs 
                                        -- as is!
        |   otherwise           = p:cs  -- and if c does know p, then p is part
                                        -- of the clique.
        where
            c = head cs                 
    -- This is the tricky part here: How come we comapre only to the head of the list?!?
    -- The reason: We KNOW there is a clique. So if it works the basic condition, we say this 
    -- is it. B/C if this is not the right clique, we are bound to find this shortly.
    -- This is what they mention: We get linear-time by KNOWING there is a clique. If we had 
    -- to consider also the non-clique case, we would have to check it through...
--
main = do
    putStr "Direct Brute-Force  method: "
    print $ cclique' ps
    putStr "Linear time method        : "
    print $ cclique' ps
