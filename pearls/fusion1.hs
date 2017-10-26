{- 
Relaxed fusion condition:
    f (foldr g a xs) = foldr h b xs
    where
        f a = b, and f (g x y) = h x (fy) for all x and y.

We will use very simple functions here, as this is a first example.

Assume we want to calculate something simliar to the l_2 norm of a sequence,
with the caveat of adding an intial value

Writing: sqrt  ( sum  ( x_i^2 + 9))

Direct way:
sqrt (foldr (\x y -> x^2+y) 9 xs)
Fusion way:

f a =sqrt 9 = 3 -> b
f ( g x y) = sqrt (x^2+y) =?= h x (f y) = h (x sqrt(y))
==> h a b = sqrt (a^2+b^2) -> h
-}


xs = [1,2,3,1,1] :: [Double]

main = do
    let f1 = sqrt $ foldr  (\x y -> x^2+y) 9 xs
    let f2 = foldr (\a b-> sqrt(a^2+b^2)) 3 xs

    print $ "Direct result:" ++ show f1
    print $ "Fusion result:" ++ show f2
