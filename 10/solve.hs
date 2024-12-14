-- Advent of Code 2024, Day 10
import Data.List
import Data.Maybe

main :: IO ()

main = do
  putStrLn "Parsing input..."
  contents <- readFile "input"
  let topoMap = lines contents                    -- ["3271...",...]
  let intMap = map charsToInts topoMap            -- [[3,2,7,1,...],...]
  let h = length intMap
  let w = length (head intMap)
  let allCols = concat $ take h $ repeat $ take w [0..]        -- [0,1,2,...,0,1,2,...]
  let allRows = concat $ map (take w . repeat) $ take h [0..]  -- [0,0,0,...,1,1,1,...]
  let allCoords = zip allRows allCols                          -- [(0,0),(0,1),(0,2),...]
    
  putStrLn "Running solver..."
  putStrLn "Solution #1, Sum of Trailhead Scores:"
  let trailheads = filter (atElevation intMap 0) allCoords
  let initLst = map (connectedElev intMap 1) trailheads
  let foundPeaks = map (seekPeaks intMap 2 9) initLst
  print $ sum $ map (length . dedup) foundPeaks
  
  putStrLn "Solution #2, Sum of Trailhead Ratings (# distinct trails):"
  print $ sum $ map length foundPeaks -- Same, but each trail counts (don't deduplicate)

readInt :: String -> Int
readInt = read

dedup :: (Ord a) => [a] -> [a]
dedup = map head . group . sort

charsToInts :: String -> [Int]
charsToInts "" = []
charsToInts xs = (readInt $ take 1 xs) : (charsToInts $ drop 1 xs)

inBounds :: Int -> Int -> (Int,Int) -> Bool
-- Returns True if (a,b) is within a grid with specified w and h
inBounds h w (a,b) = (a >= 0) && (b >= 0) && (a < h) && (b < w)

connected :: Int -> Int -> (Int,Int) -> [(Int,Int)]
-- Returns the list of all tuples physically connected to the input
connected h w (a,b) = filter (inBounds h w) unfiltered
   where unfiltered = [(a-1,b),(a,b+1),(a+1,b),(a,b-1)]

connections :: Int -> Int -> [(Int,Int)] -> [(Int,Int)]
-- List all connected nodes, excluding nodes in the input list
connections h w xs = filter (not . (`elem` xs)) unfiltered
   where unfiltered = concat $ map (connected h w) xs

elevation :: [[Int]] -> (Int,Int) -> Int
-- Return the elevation of the input coordinate in the intMap
elevation mp (row,col) = (mp !! row) !! col

atElevation :: [[Int]] -> Int -> (Int,Int) -> Bool
-- Returns True if the input point is at the specified elevation in the map
atElevation mp elev pt = (elevation mp pt) == elev

connectedElev :: [[Int]] -> Int -> (Int,Int) -> [(Int,Int)]
-- Return the list of all points at the input elevation connected to the input coordinate
connectedElev mp elev pt = filter (atElevation mp elev) $ connected h w pt
   where h = length mp
         w = length (head mp)

connectionsElev :: [[Int]] -> Int -> [(Int,Int)] -> [(Int,Int)]
-- List of all points at the input elevation connected to the input coordinates
connectionsElev mp elev xs = filter (atElevation mp elev) $ connections h w xs
   where h = length mp
         w = length (head mp)

seekPeaks :: [[Int]] -> Int -> Int -> [(Int,Int)] -> [(Int,Int)]
-- Search for connected points starting from low and reaching high
seekPeaks mp low high xs = if low == high then result else seekPeaks mp (low+1) high result
   where result = connectionsElev mp low xs
