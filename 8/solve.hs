-- Advent of Code 2024, Day 8
import Data.List

main :: IO ()

main = do
  putStrLn "Parsing input..."
  contents <- readFile "input"
  let rows = lines contents
  let w = length (head rows)
  let h = length rows
  let zeros = take h $ repeat (take w $ repeat 0)
  let freqs = ['0'..'9'] ++ ['A'..'Z'] ++ ['a'..'z']
  let ants = map (findIndices (`elem` freqs)) rows
  let antCoords = rowsToCoords 0 ants
  let antList = map (antLookup rows) antCoords -- Creates list of (freq, [xcoord,ycoord])
  
  putStrLn "Running solver..."
  putStrLn "Solution #1, Count of Unique Antinode Positions:"
  let rawANodes = concat $ map (listAntinodes antList) antList
  let antinodes = filter (not . outOfBounds h w) rawANodes
  let antinodeGrid = tallyAntinodes antinodes zeros
  print $ length $ concat $ map (filter (>0)) antinodeGrid
  
  putStrLn "Solution #2, Count of Unique Resonant Antinodes:"
  let rawLines = concat $ map (listLines h w antList) antList
  let lineGrid = tallyAntinodes rawLines zeros
  print $ length $ concat $ map (filter (>0)) lineGrid

rowToCoords :: Int -> [Int] -> [[Int]]
rowToCoords row cols = transpose [rows,cols]
   where rows = take (length cols) $ repeat row

rowsToCoords :: Int -> [[Int]] -> [[Int]]
rowsToCoords _  [] = []
rowsToCoords n  xns = (rowToCoords n (head xns)) ++ (rowsToCoords (n+1) (drop 1 xns))

antLookup :: [String] -> [Int] -> (Char, [Int])
-- Find the frequency associated with an antenna, and store the tupled info
antLookup rows coord = (freq, coord)
   where freq = (rows !! (coord !! 0)) !! (coord !! 1)

outOfBounds :: Int -> Int -> [Int] -> Bool
outOfBounds h w node = (node !! 0) < 0 || (node !! 0) >= h || (node !! 1) < 0 || (node !! 1) >= w

isHere :: [Int] -> [Int] -> Bool
isHere a b = ((a !! 0) == (b !! 0)) && ((a !! 1) == (b !! 1))

calcAntinodes :: (Char, [Int]) -> (Char, [Int]) -> [[Int]]
calcAntinodes ta tb = if (match && away) then [[xa-xab,ya-yab],[xb+xab,yb+yab]] else []
   where match = (fst ta) == (fst tb)
         away  = not $ isHere (snd ta) (snd tb)
         xa    = ((snd ta) !! 0)
         xb    = ((snd tb) !! 0)
         xab   = xb - xa
         ya    = ((snd ta) !! 1)
         yb    = ((snd tb) !! 1)
         yab   = yb - ya

listAntinodes :: [(Char,[Int])] -> (Char,[Int]) -> [[Int]]
listAntinodes []   _   = []
listAntinodes xant ant = (calcAntinodes (head xant) ant) ++ (listAntinodes (drop 1 xant) ant)

incElem :: [Int] -> [[Int]] -> [[Int]]
incElem coord xlog = (take row xlog) ++ [(take col modRow) ++ [(modRow !! col)+1] ++ (drop (col+1) modRow)] ++ (drop (row+1) xlog)
   where row = coord !! 0
         col = coord !! 1
         modRow = xlog !! row

tallyAntinodes :: [[Int]] -> [[Int]] -> [[Int]]
tallyAntinodes []  xlog = xlog
tallyAntinodes xan xlog = tallyAntinodes (drop 1 xan) newLog
   where newLog = incElem (head xan) xlog


stepSize :: [Int] -> [Int] -> [Int]
-- stepSize [3,5] [2,6] ==> [-1,1]
-- stepSize [0,7] [15,19] ==> [5,4]
stepSize orgn dest = [dx `div` factor, dy `div` factor]
   where dx = (dest !! 0) - (orgn !! 0)
         dy = (dest !! 1) - (orgn !! 1)
         factor = gcd dx dy

nextCoord :: [Int] -> [Int] -> [Int]
nextCoord orgn step = [x+dx,y+dy]
  where x  = orgn !! 0
        y  = orgn !! 1
        dx = step !! 0
        dy = step !! 1

coordsThrough :: Int -> Int -> [Int] -> [Int] -> [[Int]]
coordsThrough h w orgn step = if oob then [] else orgn : (coordsThrough h w next step)
   where oob = outOfBounds h w orgn
         next = nextCoord orgn step

calcLine :: Int -> Int -> (Char,[Int]) -> (Char,[Int]) -> [[Int]]
calcLine h w ta tb = if (match && away) then (coordsThrough h w orgnCoord step) else []
   where match = (fst ta) == (fst tb)
         away = not $ isHere orgnCoord destCoord
         orgnCoord = snd ta
         destCoord = snd tb
         step = stepSize orgnCoord destCoord

listLines :: Int -> Int -> [(Char,[Int])] -> (Char,[Int]) -> [[Int]]
listLines h w []   _   = []
listLines h w xant ant = (calcLine h w (head xant) ant) ++ (listLines h w (drop 1 xant) ant)
