{-
Square on the Inside
Problem 504
Let ABCD be a quadrilateral whose vertices are lattice points lying on the coordinate axes as follows:

A(a, 0), B(0, b), C(−c, 0), D(0, −d), where 1 ≤ a, b, c, d ≤ m and a, b, c, d, m are integers.

It can be shown that for m = 4 there are exactly 256 valid ways to construct ABCD. 
Of these 256 quadrilaterals, 42 of them strictly contain a square number of lattice points.

How many quadrilaterals ABCD strictly contain a square number of lattice points for m = 100?

Solution:

Had to google "Polygon area lattice", and immediatly Pick's theorem comes up.
https://en.wikipedia.org/wiki/Pick%27s_theorem
Very cool one, AND the proof is really interesting (inductive Triangles based.)

We have 4 triangles we will solve for.
Let's start with one:

(0,0) (a,0) (0,b)

Pick's theorem
A = i + b'/2 -1
 where A=Area, i=interior-points, and b'=boundary-points

We are interested in i
i = A-b'/2 + 1

We know:
A  = (a*b)/2
Numer of boundary-points:
    a+1 on x axis ; b+1 on y axis ; we cuonted the origion twice in those.
    gcd(a,b) - 1   points on the joining line, w/o the two end ones.
b' = a + b + 1 + { gcd(a,b)-1 } 

==>   i = (a*b)/2  - { (a+b+1)+ gcd(a,b)-1 }/2 +1
        = {a*b -a - b - gcd (a,b)}/2

We need to do the same for ALL triangles, and then add the axis-points:

i_all   = {ab + bc + cd + da -gcd(a,b)-gcd(b,c)-gcd(c,d)-gcd(d,a) -2a-2b-2c-2d}/2 + a+b+c+d+1
        = {ab + bc + cd + da -gcd(a,b)-gcd(b,c)-gcd(c,d)-gcd(d,a) }/2 +  1


"Brute-force"
Num = 694687

Then, did a few things:
Measure time.
Improve by using "sqrt" and "gcd" precomputer for all pairs (x,y).
(x always smaller, and keep as array)
-}

module E504 where

import System.CPUTime

import Data.Array

m = 100 :: Int

-- turn out, prelude has gcd!
{-
gcd         ::  Int -> Int -> Int
gcd a b     =   gcd' (max a b ) (min a b )

gcd' a b    =   if (a `rem` b) == 0 then b
                                    else gcd' a b-1
-}

picoSecond = 1e12 :: Double



main = do 
    start <- getCPUTime
    putStrLn $ "Brute force: Num of squares:" ++ (show $ findSquares m)
    end <- getCPUTime
    print $ show (fromIntegral (end-start)/picoSecond) ++ " [sec]"

    start <- getCPUTime
    putStrLn $ "Memoization: Num of squares:" ++ (show $ findSquares' m)
    end <- getCPUTime
    print $ show (fromIntegral (end-start)/picoSecond) ++ " [sec]"



findSquares     ::  Int -> Int
findSquares m   =   length $ [ (a,b,c,d) | a<-[1..m],b<-[1..m],c<-[1..m],d<-[1..m],
                            perfectSq  (pointsIn (a,b,c,d))]


perfectSq   :: Int -> Bool
perfectSq q =  sq == fromIntegral (round sq) 
                where
                    sq = sqrt (fromIntegral q) :: Double


pointsIn            :: (Int,Int,Int,Int) -> Int 
pointsIn (a,b,c,d)  = ( a*b + b*c + c*d + d*a 
                        - (gcd a b) - (gcd b c) - (gcd  c d) - (gcd d a) ) `div` 2 +1
 

-- memoization
gcdArray        = array ((1,1),(m,m)) $ concat [ [ ((i,j), gcd i j ) | j<-[1..m] ] | i<-[1..m]]
perfectSqArray  = array (1,l) $  [ (i,perfectSq i ) | i<-[1.. l] ]
                    where l = (2*m+1)*(2*m+1)

findSquares' m   =   length $ [ (a,b,c,d) | a<-[1..m],b<-[1..m],c<-[1..m],d<-[1..m],
                            perfectSqArray! (pointsIn' (a,b,c,d))]

pointsIn' (a,b,c,d)  = ( a*b + b*c + c*d + d*a 
                        - (gcdArray!(a,b)) - (gcdArray!(b,c)) 
                        - (gcdArray!(c,d)) - (gcdArray!(d,a)) ) `div` 2 +1


