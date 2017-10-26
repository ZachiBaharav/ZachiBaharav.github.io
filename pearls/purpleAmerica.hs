module Main where

-- Debug
import Debug.Trace

-- Files
import System.IO
import System.Directory

import Data.List.Split

-- types and 
type Coord    = (Double,Double)

data Region =   Region { 
    name      :: String,
    coords    :: [Coord],
    color     :: (Int,Int,Int)  -- 0 to 255
    } 
    deriving (Show)
{-
instance Show Region where
    show r = "*******\n" ++ show (name r) ++ " , \nLocation:" ++ (show (coords r) ) ++ "\n\n"
-}

outfln = "purpleMap.html"
inflnGeom  = "usa.txt"
inflnVote  = "usa2012.txt"

-- Screen size
h = 500 :: Double 
w = 500 :: Double 

-- colors
whiteColor = (255,255,255) :: (Int,Int,Int)

-- main

main :: IO()
main = do     
      contents <- readFile inflnGeom
      let (latMin, latMax, longMin, longMax, contents1) = extractBoundaries contents
      let (scale,offsetW,offsetH) = computeScale latMin latMax longMin longMax h w
      let (numOfRegions, contents2) = extractNumOfRegions contents1
      let regions = readRegions numOfRegions contents2
      let regionsScaled = scaleRegions regions (scale,offsetW,offsetH)

      contents2 <- readFile inflnVote
      let ls = drop 1 $ lines contents2 -- Skip firsl line
      let regionsScaledColored = colorRegions regionsScaled ls

      let svgImg = regionsToSVG regionsScaledColored 
      writeFile outfln svgImg
      print $ "Wrote ourput file " ++ outfln

-- functions

extractBoundaries :: String -> (Double,Double,Double,Double,String)
extractBoundaries str = (latMin,latMax,longMin,longMax,str1) 
  where
    ws = words str
    longMin = read (ws!!0)
    longMax = read (ws!!2)
    latMin  = read (ws!!1)
    latMax  = read (ws!!3)
    str1 = unlines $ drop 2 (lines str)

computeScale :: Double -> Double -> Double -> Double -> Double -> Double -> (Double,Double,Double)
computeScale latMin latMax longMin longMax h w = (scale, longMin, latMin)
  where
    scaleh = h / (latMax-latMin)
    scalew = w / (longMax - longMin)
    scale = min scaleh scalew

scaleRegions :: [Region] -> (Double,Double,Double) -> [Region] 
scaleRegions (r:rs)  s = [scaleRegion r s] ++ scaleRegions rs s 
scaleRegions [] _ = []

scaleRegion :: Region -> (Double,Double,Double) -> Region
scaleRegion r s = Region (name r) (scaleCoords (coords r) s) whiteColor

scaleCoords :: [Coord] -> (Double,Double,Double) -> [Coord]
scaleCoords [] _ = []
scaleCoords (c:cs) s@(scale,offsetW,offsetH) = [(x,y)] ++ scaleCoords cs s
  where
    x = ((fst c) - offsetW )*scale
    y = h - ((snd c) - offsetH )*scale

extractNumOfRegions :: String ->(Int,String)
extractNumOfRegions str = (n,str1)
  where
    ws = words str
    n = read (ws!!0)
    str1 = unlines $ drop 1 (lines str)



readRegions :: Int -> String -> [Region]
readRegions 0 _    = []
readRegions n str  = [oneRegion] ++ readRegions (n-1) str1
  where
    (oneRegion,str1) = readRegion str


readRegion :: String -> (Region, String)
readRegion str = (Region name coords whiteColor,str1)
  where
    ls = lines str
    name = ls!!1  -- we need to consume the empty line before.
    n = read (ls!!3) :: Int
    ws = words $ unlines $ take n ( drop 4 ls)
    coords = splitWsLongLat ws
    str1 = unlines $ drop (n+4) ls

splitWsLongLat :: [String]->[(Double,Double)]
splitWsLongLat (x1:x2:xs) = [(read x1,read x2)] ++ splitWsLongLat xs
splitWsLongLat _ = []  


colorRegions :: [Region]->[String]->[Region]
colorRegions rs (l:ls) = [colorRegion rs l] ++ colorRegions rs ls 
colorRegions _ [] = []

colorRegion :: [Region] -> String -> Region
colorRegion rs l = Region name cs color
  where
    lparts = splitOn "," l
    name = lparts!!0
    region = findRegionByName rs name
    cs = (coords region)
    (r,d,i) = (read (lparts!!1),read (lparts!!2),read (lparts!!3)) :: (Int,Int,Int)
    sum = fromIntegral (r+d+i) :: Double
    color = ( floor ( (fromIntegral r)/sum * 255),
              floor ( (fromIntegral i)/sum * 255),
              floor ( (fromIntegral d)/sum * 255) ) :: (Int,Int,Int)

findRegionByName :: [Region] -> String -> Region
findRegionByName (r:rs) nom = if ( (name r) == nom) then r 
                                                    else findRegionByName rs nom
findRegionByName [] nom   = Region "" [] whiteColor -- should never get here... 


regionsToSVG :: [Region] -> String
regionsToSVG rs = headerStr ++ str1 ++ footerStr 
  where
    headerStr = "<!DOCTYPE html><html><body><h1>PurpleMap SVG</h1><svg width=\"500\" height=\"500\">"
    --str1 = foldl roomToSVG "" rs
    str1 = regionsToSVG' rs
    footerStr = "</svg></body></html>"


regionsToSVG' :: [Region] -> String
regionsToSVG' (r:rs) = (regionToSVG r) ++ regionsToSVG' rs
regionsToSVG' []     = []

regionToSVG :: Region -> String
regionToSVG r = svgPolygon (coords r) (color r)



--  <polygon points="220,10 300,210 170,250 123,234" style="fill:lime;stroke:black;stroke-width:1" />" 
-- for color: style="fill:rgb(0,0,255);stroke-width:3;stroke:rgb(0,0,0)"
svgPolygon :: [Coord] -> (Int,Int,Int) -> String
svgPolygon xs c= "<polygon points=\"" ++ coordsToString xs ++ "\" style=\"fill:rgb" ++ show c ++  ";stroke:black;stroke-width:1\" />"


coordsToString :: [Coord] -> String
coordsToString [] = ""
coordsToString (x:xs) = show (fst x) ++ "," ++ show (snd x) ++ " "  ++ coordsToString xs


  -- end