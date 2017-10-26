{-

Problem:
Create a map of conencted rooms.
Requirements:
(a) You can get from any room to any other.
(b) Room has between 0 to 4 doors.
(c) No door leads to 'abyss'.

Background:
This project was needed for a simple (up to 16) room-map for a game.

Solution:
The algorithm here would be referred to as 'brute force'.
We solve it recursively, by adding one room at a time.

We start with one room with no doors.

The function that ADDS a room works as follows:
1. enumerate all the possible room locations: Those that are neighbours to existing rooms,
and do not overlap with existing rooms.
2. Randomly choose one of these locations.
3. Add door(s) to conenct it to the rest of the rooms. Right now: Just add a door to the 
room itself, and one of it's neighbours.

I also used SVG to demonstrate creating graphics in the browser using text-strings.
----

Note I: This solution is very verbose, including debug statements and all.
It was passed to a student who does-not know Haskell, and was implemented
later on in Objective-C.

Note II: VERY not optimized, nor full. For example, can use list of available spots and update it.
Example: opens only one-door to one neighbour.

-}

module Main where
-- Random 
import System.Random (getStdRandom, randomR)
import System.IO.Unsafe (unsafePerformIO)

-- Debug
import Debug.Trace

-- types and data
type Loc   = (Int,Int)
data Room =   Room { 
    rId             :: Int,
    rLoc            :: Loc,
    rDoors          :: [Bool]    -- NSEW
    } 

instance Show Room where
    show r = "(" ++ show (rId r) ++ " , Location:" ++ (show (rLoc r) )++ 
             "Doors at (NSEW): " ++ show (rDoors r) ++ ")\n"


-- initlaize and helpers
numberOfRooms = 1000 ::Int
noDoors       = [False,False,False,False]

outfln = "rooms.html"

-- main
main :: IO()
main =  do
  let mapRooms = createMap numberOfRooms []
  print mapRooms
  let svgImg = roomsToSVG mapRooms
  writeFile outfln svgImg

-- functions
-- Create map: how many rooms to add, and current list of rooms. Returns 
-- new list of rooms.
createMap :: Int -> [Room] -> [Room]
createMap 0 rs  = rs                                       -- no more rooms to add
createMap n []  = createMap (n-1)  [Room 0 (0,0) noDoors ] -- Adding first room!
createMap n rs  = createMap (n-1)  (addRoom rs)            -- Adding a room

-- adding a single room:
addRoom :: [Room] -> [Room]
addRoom rs = newRs
  where
    candidateLocs = freeNeighbours rs
    locChosen     = chooseOne candidateLocs
    newRs         = addDoors rs locChosen 


freeNeighbours :: [Room] -> [Loc]
freeNeighbours rs = removeExisting rs (allPossibleNeighbours rs)

allPossibleNeighbours :: [Room] -> [Loc]
allPossibleNeighbours []      = []
allPossibleNeighbours (r:rs)  = (createFourRooms r ) ++ (allPossibleNeighbours rs)

createFourRooms :: Room -> [Loc]
createFourRooms r = [rN,rS,rE,rW]
  where
    (x,y) = rLoc r
    rN = (x,y+1)
    rS = (x,y-1)
    rE = (x+1,y)
    rW = (x-1,y)


removeExisting :: [Room] -> [Loc] -> [Loc]
removeExisting rs locs = removeExisting' locRooms locs
  where
    locRooms = map (\x -> (rLoc x)) rs

removeExisting' :: [Loc] -> [Loc] -> [Loc]
removeExisting' locRooms [] = []  
removeExisting' locRooms (loc:locs) = ll ++ (removeExisting' locRooms locs)  
  where
    ll = if (loc `elem` locRooms) then [] else [loc]

-- removing duplicate
-- removeExisting rs locs = 
-- foldl (\seen x -> if x `elem` seen then seen else seen ++[x]) [] locs

chooseOne :: [Loc] -> Loc
chooseOne locs = locs !! randOne
  where
    randOne = (dieRoll (length(locs)) ) -1
--chooseOne locs = head (locs)

-- We need to add doors to the current new-room,
-- and to it's neighbour(s) in the already
-- existing list.
-- Right now, only adding one door to one-neighbour.
addDoors :: [Room] -> Loc -> [Room]
addDoors rs loc = rnew' : r1' :rs'
  where
    rnew  = Room 0 loc noDoors  
    r1    = findNeighbour rnew rs 
    rnew' = addDoor rnew r1
    r1'   = addDoor r1 rnew
    rs'   = removeRoom rs r1

findNeighbour :: Room -> [Room] -> Room
findNeighbour rnew (r:rs) = if (neighbour rnew r) then r else findNeighbour rnew rs


neighbour :: Room -> Room -> Bool
neighbour r1 r2 = if nsew then True else False
  where 
    x1 = fst $ rLoc r1
    y1 = snd $ rLoc r1
    x2 = fst $ rLoc r2
    y2 = snd $ rLoc r2
    nsew = (x1==x2 && y1==y2+1) || (x1==x2 && y1==y2-1) || 
      (x1==x2+1 && y1==y2) || (x1==x2-1 && y1==y2)


addDoor :: Room -> Room -> Room
addDoor r1 r2 
  | (x1==x2 && y1==y2+1) = r1 {rDoors =  (head iDoors : True : (drop 2 iDoors) )}       -- y1 is North
  | (x1==x2 && y1==y2-1) = r1 {rDoors =  (True : (drop 1 iDoors) )  }                   -- y1 is South
  | (x1==x2+1 && y1==y2) = r1 {rDoors =  ((take 3 iDoors) ++ [True] ) }                   -- x1 is East
  | (x1==x2-1 && y1==y2) = r1 {rDoors =  ((take 2 iDoors) ++ [True] ++ (drop 3 iDoors)) }   --x1 is West
  where 
    x1 = fst $ rLoc r1
    y1 = snd $ rLoc r1
    x2 = fst $ rLoc r2
    y2 = snd $ rLoc r2
    iDoors = rDoors r1

removeRoom :: [Room] -> Room -> [Room]
removeRoom rs r = filter (\x -> (rLoc x) /= loc) rs
  where 
    loc = rLoc r

-- Standard way of getting a random integer number in range [1,sides]
dieRoll :: Int -> Int
dieRoll sides = 
  let retval = unsafePerformIO roll_a_die 
      roll_a_die = getStdRandom $ randomR (1, sides)
      msg = show (sides, retval)
  in trace msg retval


roomsToSVG :: [Room] -> String
roomsToSVG rs = headerStr ++ str1 ++ footerStr 
  where
    headerStr = "<!DOCTYPE html><html><body><h1>Room SVG</h1><svg width=\"600\" height=\"600\">"
    str1 = foldl roomToSVG "" rs
    footerStr = "</svg></body></html>"

roomToSVG :: String -> Room -> String
roomToSVG str r = str ++ strC ++ strN ++ strS ++ strE ++ strW
  where
    x = fst (rLoc r)
    y = snd (rLoc r)
    rr = rDoors r
    strC = svgCircle x y
    strN = if (rr!! 0) then (svgLine x y (fromIntegral x) ((fromIntegral y)+0.5)) else ""
    strS = if (rr!! 1) then (svgLine x y (fromIntegral x) ((fromIntegral y)-0.5)) else ""
    strE = if (rr !! 2) then (svgLine x y ((fromIntegral x)+0.5) (fromIntegral y)) else ""
    strW = if (rr !! 3) then (svgLine x y ((fromIntegral x)-0.5) (fromIntegral y)) else ""

scale = 10 :: Double
offset = 300 :: Double

svgCircle :: Int->Int->String
svgCircle x y = "<circle cx=\" " ++ show x' ++ "\" cy=\"" ++ show y' ++ "\" r=\"4\" stroke=\"green\" stroke-width=\"1\" fill=\"yellow\" />"
  where
    x'=(fromIntegral x)*scale + offset
    y'=(fromIntegral y)*scale + offset
 
svgLine :: Int -> Int -> Double -> Double -> String
svgLine x y w z = "<line x1=\"" ++  show x' ++ "\" y1=\"" ++ show y' ++ "\" x2=\"" ++ show w' ++ "\" y2=\"" ++ show z' ++ "\" style=\"stroke:rgb(255,0,0);stroke-width:2\" />"
  where
    x'=(fromIntegral x)*scale + offset
    y'=(fromIntegral y)*scale + offset
    w'=w*scale + offset
    z'=z*scale + offset

  -- end