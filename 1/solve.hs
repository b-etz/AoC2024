-- Advent of Code 2024, Day 1
import Data.List

main :: IO ()

main = do
  putStrLn "Parsing input..."
  contents <- readFile "input"
  let lists = transpose ( map (map readInt . words) (lines contents) )
  
  putStrLn "Running solver..."  
  let pairs = transpose ( map sort lists )
  let diffs = map absDiff pairs
  putStrLn "Solution #1, Difference Score:"
  print $ sum diffs
  
  putStrLn "Solution #2, Similarity Score:"
  print $ sumProd (lists !! 0) (countify lists)
  
  
readInt :: String -> Int
-- Read the input numeric String into an Int
readInt = read


absDiff :: [Int] -> Int
-- Return the difference between the first and second Ints in [Int]
absDiff xs = abs ( (xs !! 0) - (xs !! 1) )


countify :: [[Int]] -> [Int]
-- For each item in the left column of [[Int]], return the number of times
--    it appears in the right column of [[Int]]]
countify ll = map (countMap (ll !! 1)) (ll !! 0)
countMap xs key = length (filter (\x -> x == key) xs)


sumProd :: [Int] -> [Int] -> Int
-- Dot-multiply all elements in List A with List B and sum the result
sumProd [] _  = 0
sumProd _  [] = 0
sumProd lA lB = (head lA) * (head lB) + sumProd (drop 1 lA) (drop 1 lB)