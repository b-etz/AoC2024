-- Advent of Code 2024, Day 6
import Data.List

main :: IO ()

main = do
  putStrLn "Parsing input..."
  contents <- readFile "input"
  let initMap = lines contents
  let w = length (head initMap)
  let h = length initMap
  let initVisited = take h $ repeat (take w $ repeat 0) -- (h x w) grid of zeros
  let obstacles = map (elemIndices '#') initMap -- search by: col `elem` (obstacles !! row)
  let marco = map (findIndices (`elem` "^>v<")) initMap
  let polo = concat marco
  let initCoord = (findIndices (not . null) marco) ++ polo
  let initDir = arrowToDir $ (initMap !! (initCoord !! 0)) !! (initCoord !! 1)
  
  putStrLn "Running solver..."
  let finalLog = advanceState obstacles initVisited initCoord initDir
  putStrLn "Solution #1, Count of Distinct Positions:"
  print $ length $ filter (>0) (concat finalLog)
  
  putStrLn "Solution #2, Count of Infinite Loop Positions:"
  let allIndices = take h $ repeat [0..(w-1)]
  let newObs = filterAfromB obstacles allIndices
  let newObsPairs = makePairsRows 0 newObs -- Must exclude initCoord (follow-up: doesn't form loop)
  let loops = map (isLoop initVisited initCoord initDir) (map (insertObs obstacles) newObsPairs)
  print $ length $ filter (\x -> x) loops

arrowToDir :: Char -> [Int]
-- Convert an initial symbol to 
arrowToDir xs = case xs of {
    '^' -> [-1, 0];
    '>' -> [ 0, 1];
    'v' -> [ 1, 0];
    '<' -> [ 0,-1];
    }

filterAfromB :: [[Int]] -> [[Int]] -> [[Int]]
-- Return a list of lists, with all elements in row i of A removed from row i of B
filterAfromB _  [] = []
filterAfromB [] lB = lB
filterAfromB lA lB = (filter (not . (`elem` (head lA))) (head lB)) : (filterAfromB (drop 1 lA) (drop 1 lB))

makePairsRow :: Int -> [Int] -> [[Int]]
makePairsRow row cols = transpose [rows,cols]
    where rows = take (length cols) $ repeat row

makePairsRows :: Int -> [[Int]] -> [[Int]]
makePairsRows _  [] = []
makePairsRows n xns = (makePairsRow n (head xns)) ++ (makePairsRows (n+1) (drop 1 xns))

insertObs :: [[Int]] -> [Int] -> [[Int]]
-- Insert a [row, col] obstacle into a list of obstacles [[col,col2,...],[col1,col2,...],...]
insertObs obs new = (take row obs) ++ [(new !! 1) : modList] ++ (drop (row+1) obs)
    where row = new !! 0
          modList = obs !! row


dirToInt :: [Int] -> Int
-- Generate an integer corresponding to each direction
dirToInt xs = 3 + 2*(xs !! 0) - (xs !! 1)

logDir :: [[Int]] -> [Int] -> [Int] -> [[Int]]
logDir log dir coord = (take row log) ++ [(take col modList)++[elem]++(drop (col+1) modList)] ++ (drop (row+1) log)
    where row = coord !! 0
          col = coord !! 1
          modList = log !! row
          elem = dirToInt dir

outOfBounds :: Int -> Int -> [Int] -> Bool
outOfBounds h w dest = (dest !! 0) < 0 || (dest !! 0) >= h || (dest !! 1) < 0 || (dest !! 1) >= w

isObstacle :: [[Int]] -> [Int] -> [Int] -> Bool
-- Returns True if the provided direction is an obstacle
isObstacle obs coord dir = if (outOfBounds w w dest) then False else (dest !! 1) `elem` (obs !! (dest !! 0))
    where dest = zipWith (+) coord dir
          w = length obs

toggleDir :: [Int] -> [Int]
-- Swing the direction to the right
-- e.g.: [-1, 0] ('^') ==> [0, 1] ('>')
toggleDir dir = [(dir !! 1), -(dir !! 0)]

updateDir :: [[Int]] -> [Int] -> [Int] -> [Int]
-- Take an obstacle list, a coordinate, and a former direction
--    If possible, maintain old direction. Otherwise, toggle the direction and reassess
updateDir obs coord dir = if dOh then updateDir obs coord (toggleDir dir) else dir 
    where dOh = isObstacle obs coord dir

advanceState :: [[Int]] -> [[Int]] -> [Int] -> [Int] -> [[Int]]
-- ObstacleList, VisitorLog, Origin, Direction ==> NewLog
-- Update VisitorLog with the Origin
-- If the new destination is out of bounds, break
-- Else, feed the new destination to "advanceState" as the new Origin, with an updated direction
advanceState obs log orgn dir = if (outOfBounds h w dest) then newLog else advanceState obs newLog dest (updateDir obs dest dir)
    where w = length (head log)
          h = length log
          newLog = logDir log dir orgn
          dest = zipWith (+) orgn dir


logVal :: [[Int]] -> [Int] -> Int
-- Return the integer in the log at the coordinate
logVal log coord = row !! (coord !! 1)
    where row = log !! (coord !! 0)

isLoop :: [[Int]] -> [Int] -> [Int] -> [[Int]] -> Bool
isLoop log orgn dir obs = if (outOfBounds h w dest) then False else loop || (isLoop newLog dest (updateDir obs dest dir) obs)
   where h = length log
         w = length (head log)
         newLog = logDir log dir orgn
         dest = zipWith (+) orgn dir
         loop = (logVal log orgn) == (dirToInt dir) -- If it's a loop, we're tracing our own previous path
