-- Advent of Code 2024, Day 4
import Data.List

main :: IO ()

main = do
  putStrLn "Parsing input..."
  contents <- readFile "input"
  let lR = lines contents
  let rL = map reverse lR
  let uD = transpose lR
  let dU = map reverse uD
  
  putStrLn "Running solver..."  
  putStrLn "Solution #1, XMAS Count:"
  print $ (countXmasR lR) + (countXmasR rL) + 
    (countXmasR uD) + (countXmasR dU) + 
    (countXmasD lR) + (countXmasD rL) + 
    (countXmasD $ reverse lR) + (countXmasD $ reverse rL)
  
  putStrLn "Solution #2, X-MAS Count:"
  let all3x3s = concat (map listNxNs (setsOfExact 3 lR))
  print $ sum $ map valXMAS all3x3s


isXmas :: String -> Bool
isXmas str = ("XMAS" == take 4 str)

countXmas :: String -> Int
-- Return the number of times "XMAS" appears uniquely in the String
countXmas "" = 0
countXmas str = countXmas (drop 1 str) + ( if isXmas str then 1 else 0 )


setsOfN :: Int -> [a] -> [[a]]
-- Return a list of up-to-N element lists, of consecutive items from [a]
setsOfN _ [] = []
setsOfN n xs = (take n xs) : setsOfN n (drop 1 xs)

setsOfExact :: Int -> [a] -> [[a]]
-- Do the above, but enforce N elements per inner list
setsOfExact n xs = filter (\x -> n == length x) (setsOfN n xs)


triangleShift :: [[a]] -> [[a]]
-- Drops idx 0 by 0, idx 1 by 1, etc.
triangleShift xs = [ drop n (xs !! n) | n <- [0..length(xs)-1] ]

countXmasD :: [String] -> Int
-- Returns the number of times "XMAS" appears diagonally in the [String], like:
-- .X....
-- ..M...
-- ...A..
-- ....S.
countXmasD xs = sum $ map sum $ map (map countXmas) ll_candidates
    where ll_candidates = map transpose $ map triangleShift (setsOfExact 4 xs)

countXmasR :: [String] -> Int
-- Returns the number of times "XMAS" appears in the [String]
countXmasR xs = sum $ map countXmas xs


listNxNs :: [[a]] -> [[[a]]]
-- Given a grid with N rows, return every grid with N adjacent columns
listNxNs xs = transpose $ map (setsOfExact n) xs
    where n = length xs

valXMAS :: [String] -> Int
-- Given a grid of 3x3 characters, assign 1 to any grid like:
--   M . M   S . S   M . S   S . M
--   . A .   . A .   . A .   . A . 
--   S . S   M . M   M . S   S . M
valXMAS xs = (vX xs) + (vX rxs) + (vX txs) + (vX rtxs)
    where rxs = reverse xs
          txs = transpose xs
          rtxs = reverse ( transpose xs )

vX :: [String] -> Int
-- Given a grid of 3x3 characters, assign 1 to ONLY:
--   M . M
--   . A .
--   S . S
vX xs = if ('M' == head cc) && ('M' == cc !! 2) && ('A' == cc !! 4) && ('S' == cc !! 6) && ('S' == last cc) then 1 else 0
    where cc = concat xs


