-- jumble.hs

module Main where

import System.Environment (getArgs)
import Data.List (isPrefixOf, delete)


jmbl = "calling"
wordList = ["abs","ball","cake","call","calling","calm","car"]

-- solve jmbl   -> wordslist -> answer
solve :: String -> [String]  -> [String]
solve jmbl wordsList = solvePrefix wordsList ("",jmbl)

-- solvePrefix wordsLeft   (prefix,remainder)  -> [solutions]
solvePrefix :: [String] -> (String,String)     -> [String]
solvePrefix [] (_,_)              = []
solvePrefix wordsLeft (prefix,[]) = if  (prefix `elem` wordsLeft)  then [prefix] else []
solvePrefix wordsLeft (prefix,remainder) = foldl  (++) [] $ map (solvePrefix updatedWords) newPrefixs
          where
            newPrefixs   = [(x,y)  | (x,y)<- (prefixUpdate prefix remainder) ]
            updatedWords = updateWords prefix wordsLeft


-- updateWords prefix -> wordsLeft -> updatedWords
updateWords :: String -> [String]  -> [String]
updateWords _ [] = []
updateWords [] w = w
updateWords p w  = filter (isPrefixOf p) w



-- prefixUpdate prefix -> remainder -> [(prefixUpdated, remainderUpdated)]
prefixUpdate :: String -> String    -> [(String,String)]
prefixUpdate prefix remainder = 
        [ (prefix ++ [nextChar], delete nextChar remainder) | nextChar <- remainder]

-- Note for prefixUpdate: It doesn't matter that 'delete' always drop off only
-- the first occurence. The important thing is that is drops only ONE
-- occurence



main :: IO ()
main = do args <- getArgs
          let (jmbl,fln) = case args of
                [j,f] -> (j,f)
                _ -> error "Need two arguments: jumbleString and dictionaryName"
          text <- readFile fln
          let wordsList = words text
          print $ solve jmbl wordsList


