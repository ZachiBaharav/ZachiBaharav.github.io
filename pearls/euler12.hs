{-
Highly divisible triangular number

Problem 12
The sequence of triangle numbers is generated by adding the natural numbers. 
So the 7th triangle number would be 1 + 2 + 3 + 4 + 5 + 6 + 7 = 28. 
The first ten terms would be:
1, 3, 6, 10, 15, 21, 28, 36, 45, 55, ...

Let us list the factors of the first seven triangle numbers:

 1: 1
 3: 1,3
 6: 1,2,3,6
10: 1,2,5,10
15: 1,3,5,15
21: 1,3,7,21
28: 1,2,4,7,14,28
We can see that 28 is the first triangle number to have over five divisors.

What is the value of the first triangle number to have over five hundred divisors?

Solution:
Brute force.

-}

-- answer:  76576500

module E12 where 



numOfDivisors' :: Integer -> Int
numOfDivisors' n = 2* (divisors' n) + isSquare
    where
        sn = sqrt (fromIntegral n)
        ul = floor sn
        divisors' n = length $ filter (==0) $ map (\x->n `rem` x) [1..ul-1] 
        isSquare = if ul*ul == n then 1 else 0

tNums = scanl (+) 1 [2..]
theList =  zip (map numOfDivisors' tNums) tNums

main = do
    print $ head  [ y | (x,y)<-theList, x>5]
    print $ head  [ y | (x,y)<-theList, x>500]

