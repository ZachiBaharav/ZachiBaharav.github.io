{-

Problem 493
70 colored balls are placed in an urn, 10 for each of the seven rainbow colors.
What is the expected number of distinct colors in 20 randomly picked balls?

Give your answer with nine digits after the decimal point (a.bcdefghij).

Solution

There are so many solutions (and some of them give the same answer!!)
The funny thing is that this problem can be solved very simply
analytically...

Let's create an indicator function which is 'X_i' to indicate if
    color i is in the mix.

We are looking for:
E{ X_1 + X_2 +..+ X_7} =
Linearity 
E{X_1} + E{X_2} + ...  =
symmetry
7*E{X_1} =
The probbaility of NOT having X_1 : (20 choose 60)/(20 choose 70)
Thus, the probbaility of X_1 is (1- theAbove).
And, since X_1={0,1}, we get
7 * (1 - (20 choose 60)/(20 choose 70) )

And that's it!!

Just a few notes before hand:
 - Integers in Haskell are arbitrary-precision integers, so hopefully 
 no need to worry abuot that.
 - The function choose is given below, with two implemntations

Answer: 6.818741802
-}
module Main where


probOneOut = (fromIntegral (choose 60 20)) /  (fromIntegral (choose 70 20) )
probOneIn = 1 - probOneOut
meanColors = 7*probOneIn

main = do
    print $ meanColors 

-- For small numbers, this is clearer.
choose :: (Integral a) => a -> a -> a
choose n k = product [k+1..n] `div` product [1..n-k]


{-
For large n, if we want to avoid error with large numbers, we
    can do it iteratively:
-}
choose' :: (Integral a) => a -> a -> a
choose' n k = foldl (\z i -> (z * (n-i+1)) `div` i) 1 [1..k]

