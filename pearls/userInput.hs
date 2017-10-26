-- User input

module Main where


main :: IO()
main = do
    putStrLn "Hellow world. (wil be printed EACH time)."
    putStrLn "Please enter a number, 0 to end: "
    str <- getLine
    let num = read str
    putStrLn $ "You entered " ++ (show num)
    if num==0   then return()
                else main
                

