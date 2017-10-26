
{-

We will use this notation:
   (k-1)*k*(k+1) = k*(k^2-1) = k^3-k
and in the final result will put n=(k-1) so it corresponds to:
   n*(n+1)*(n+2)

To validate if a number is the product of three consecutive:
	P =?  k^3-k
we'll do:
    P -> (P)^1/3 -> ceil of that => q -> P =? q^3 - q ::  Yes or No

-}

module TriWizard where

t = [1..22736]
tn = tail( scanl (+) 0 t )


-- checkTri 6
-- checkTri 258474216
checkTri :: Int -> Bool
checkTri p =    p == q^3-q 
    where
        q = ceiling ((fromIntegral p)**(1.0/3)) :: Int



main = do
	let ans = filter checkTri tn 
	print ans