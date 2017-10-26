{-
Euler 28

Number spiral diagonals
Problem 28
Starting with the number 1 and moving to the 
right in a clockwise direction a 5 by 5 spiral is formed as follows:

21 22 23 24 25
20  7  8  9 10
19  6  1  2 11
18  5  4  3 12
17 16 15 14 13

It can be verified that the sum of the numbers on the diagonals is 101.

What is the sum of the numbers on the diagonals in a 1001 by 1001 spiral formed 
in the same way?


Solution

The Top-Right corner of nxn spiral is (of course) n^2, as this is the number of elements
in the formed square.

The other corners of the square can therefore be computed:
Top-Right   =   n^2
Top-Left    =   n^2 - (n-1)
Bottom-Left =   n^2 - 2*(n-1)
Bottom-Right=   n^2 - 3*(n-1)

The sum of these is for a size n spiral:
4*n^2 - 6*(n-1)
Thus, the requested sum is the sum of Spirals-corners of size 1,3,5,7,...1001 :
we'll put n=2k+1, and denote K0=500:
1 + sum_k=1^k=K0  [ 4*(2k+1)^2 - 6(2k+1-1) ] =
1 + sum_k=1^k=K0  [ 16*k^2 + 16*k + 4 - 12k ] =
1 + sum_k=1^k=K0  [ 16*k^2 + 4*k + 4 ] =
    
Let's check for n=5 ==> k=2
1 + 16*(1+4) +4*(1+2) + (4+4) = 1+80+12+8 =  101

sum {k=1..n} 1      = n
sum {k=1..n} k      = n*(n+1)/2
sum {k=1..n} k^2    = n*(n+1)*(2n+1)/6

Thus:
1 + 16 *K0*(K0+1)*(2*K0+1)/6  + 4*K0*(K0+1)/2 + 4*K0

K0=500 ==> 669171001

-}

module E28 where




k0 = 500 :: Double

main = do 
    putStrLn $ "sum=" ++ 
            (show $ round (1 + 16 *k0*(k0+1)*(2*k0+1)/6  + 4*k0*(k0+1)/2 + 4*k0)  )


