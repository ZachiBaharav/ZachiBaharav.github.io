{-
Problem 9

Special Pythagorean triplet

A Pythagorean triplet is a set of three natural numbers, a < b < c, for which,
a^2 + b^2 = c^2
For example, 3^2 + 4^2 = 5^2.

There exists exactly one Pythagorean triplet for which a + b + c = 1000.
Find the product abc.

Answer:
31875000
-}

module E9 where


--m = 12
m = 1000



main = do 
    print $  product pytTri

pytTri :: [Int] 
pytTri = concat $ [ [a,b,(m-a-b)] | a<-[1..m], b<-[1..a],  a*a+b*b == (m-a-b)*(m-a-b)] 
            -- using the fact (3,4,5) is same as (4,3,5)

