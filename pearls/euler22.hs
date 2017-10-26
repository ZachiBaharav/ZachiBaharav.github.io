{-
Names scores
Problem 22
Using names.txt (right click and 'Save Link/Target As...'), a 46K text file containing over 
five-thousand first names, begin by sorting it into alphabetical order. 

Then working out the alphabetical value for each name, multiply this 
value by its alphabetical position in the list to obtain a name score.
For example, when the list is sorted into alphabetical order, COLIN, which is worth 
3 + 15 + 12 + 9 + 14 = 53, is the 938th name in the list. 
So, COLIN would obtain a score of 938 × 53 = 49714.

What is the total of all the name scores in the file?

Solution
Score = 871198282


-}

module E22 where

import Data.List (sort)

main = do 
    txt <- readFile "p022_names.txt"
    let names = parse txt
    putStrLn $ "Name number 938 =" ++ (show $ (sort names)!!937) -- lists start from 0
    putStrLn $ "Score=" ++ (show $ score (sort names))


score       :: [String] -> Integer-+
score ns    =  sum $ [ i*(worth n)| (n,i)  <- zip ns [1..]]

worth           :: String -> Integer
worth []        = 0
worth (x:xs)    =  fromIntegral ( fromEnum x - off ) + (worth xs)
                    where off = fromEnum 'A'- 1 

parse           ::  String -> [String]
parse []        =   []
parse (x:xs)    |   x == '"'    =  [takeWhile (/='"') xs] ++ 
                                    parse (tail $ dropWhile (/='"') xs)
                |   x == ','    =   parse xs 

