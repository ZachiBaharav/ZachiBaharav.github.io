{-
Tangent Circles
Problem 510
Circles A and B are tangent to each other and to line L at three distinct points.
Circle C is inside the space between A, B and L, and tangent to all three.
Let rA, rB and rC be the radii of A, B and C respectively.

Let S(n) = Σ rA + rB + rC, for 0 < rA ≤ rB ≤ n where rA, rB and rC are integers.
The only solution for 0 < rA ≤ rB ≤ 5 is rA = 4, rB = 4 and rC = 1, so S(5) = 4 + 4 + 1 = 9.
You are also given S(100) = 3072.

Find S(10^9).

Solution:

one can do "Brute-force"

After simplifying the equations, we end with:
rc = ra*rb / (sqrt(ra)+sqrt(rb))^2
From the denominator, it means sqrt(ra*rb) = integer ==> ra*rb = perfect sqaure.
So let's say the gcd(ra,rb) =d ==> ra=d*x^2, rb=d*y^2, where (x,y) are coprime (namely, gcd(x,y)=1).

Since rb<=m,  1<=y<=sqrt(m).
Since ra<=rb, 1<=x<=y
d = 1:m
rc = d^2 * x^2* y^2 / ( d*(x+y)^2 ) =  d * x^2* y^2 / (x+y)^2 

======
For m<100 (and even m<10^3) it  holds:
it can be reasoned that the solution are of the form:
rb=4*k,     ra=rb,  rc=k, for k=1..
AND
rb=36*p,    ra=9*p, rc=4*p, for p=1...

Thus:
S =  9*k (for k=1...m/4)    +  49*p (for p=1..m/36) 
=====

-- Compilation:
ghc -fforce-recomp -prof -fprof-auto --make -O2 euler510.hs -main-is E510
(ghc --help)
(ghc --show-options)
https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/flag-reference.html

-prof  -fprof-auto  : profiling
--make              : easier for multimodule
-O2                 : level 2 optimization
-fforce-recomp      : turn off recompilation checking

euler510.exe  +RTS -p

Produces the file 
euler510.prof


Answer:
315306518862563689
-}

module E510 where

import System.CPUTime
picoSecond = 1e12 :: Double


m = 10^9 :: Integer
--m = 10^2 :: Integer



main = do 
    putStrLn "Brute force:"
    start <- getCPUTime
    print $ sOfM m
    -- print $ sOfM' m
    
    end <- getCPUTime
    print $ show (fromIntegral (end-start)/picoSecond) ++ " [sec]"


sOfM    :: Integer -> Integer
sOfM m  =  sum $ concat $ allCircs m



allCircs    :: Integer -> [[Integer]]
allCircs m  =   concat.concat $ [[[  (calcRs d x y)  | d<-[1..(m `div` (y*y))] ,isComb d x y ]  
                                 | x<-[1..y], (gcd x y) ==1] 
                                 | y<-[1..(round $ sqrt (fromIntegral m))] ]

             where
                isComb d x y = (d*x*x*y*y) `rem` ((x+y)*(x+y)) == 0 
                calcRs d x y = [ra,rb,rc]
                    where
                        ra = d*x*x
                        rb = d*y*y
                        rc = (ra*rb) `div` (d*(x+y)*(x+y))


isComb              ::  (Integer,Integer,Integer) -> Bool
isComb (ra,rb,rc)   = lhs == rhs
            where
                lhs = 4*( (ra+rc)^2 - (ra-rc)^2)*( (rb+rc)^2 - (rb-rc)^2)
                rhs = ( ((rb+ra)^2 - (rb-ra)^2) - 
                        ((ra+rc)^2 - (ra-rc)^2 +(rb+rc)^2 - (rb-rc)^2)     )^2



-------------------
q= filter (\x-> (head x) /= (head(tail x))) $ allCircs m
q'= filter (\x-> ((head(tail x)) `rem` 36) /=0) q

-- nice try.. that doesn't work
sOfM'    :: Integer -> Integer
sOfM' m  =  9*sumK + 49*sumP
        where
            k1      =   m `div` 4  :: Integer
            sumK    =   (1+k1)*k1  `div` 2 :: Integer
            p1      =   m `div` 36 :: Integer
            sumP    =   (1+p1)*p1  `div` 2  :: Integer



