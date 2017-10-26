{-
Project Euler

** Problem 1
If we list all the natural numbers below 10 that are multiples of 3 or 5, 
we get 3, 5, 6 and 9. The sum of these multiples is 23.

Find the sum of all the multiples of 3 or 5 below 1000.
-}

{-
 Solution methods:
 1. Brute force: List comprehension
 2. Series sums:
    sum_k=1^floor(999/3) (3*k) +
    sum_k=1^floor(999/5) (5*k) -
    sum_k=1^floor(999/15) (15*k)

 3. of course, we can also compute the series-sum by hand (=formula).
-}

sumk :: Integer -> Integer
sumk x = sum [i*x | i<-[1..upper]]
    where
        upper = 999 `div` x   -- easier then floor


main = do
    print $ "Brute force:"
    print $ sum [x | x<-[1..999], (x `rem` 3)==0  || (x `rem` 5)==0 ]
    print $ "Using sums:"
    print $ sumk 3  + sumk 5 - sumk 15
