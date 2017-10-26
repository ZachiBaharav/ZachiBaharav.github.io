{-
'C' like printf function:
https://hackage.haskell.org/package/base-4.7.0.1/docs/Text-Printf.html 

Similar formatting conventions.

printf can return either a String or IO()
-}


import Text.Printf

a = [5,7,9] :: [Int]

main = do

    -- Note: Printf is not too helpful with Types decisions, and very often you will get complaints.
    -- Here, for example, without the [Int] specification on the [0..] index, things wouldn't compile.
    putStr $ unlines $ [ printf "element %2d :: value = %2d" i item  | (i, item) <- zip ([0..] ::[Int]) a ]

    putStr $  ( printf "Decimal: %03d \nOctal  : %o \nHexa   : %x \nExponent: %e\n" 
                        (95 ::Int)         (7::Int)            (15::Int)       (123456::Double) )

    -- Also would work as:
    printf "Decimal: %03d \nOctal  : %o \nHexa   : %x \nExponent: %e\n" 
                        (95 ::Int)         (7::Int)            (15::Int)       (123456::Double)
