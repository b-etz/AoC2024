-- Advent of Code 2024, Day 2
import Data.List

main :: IO ()

main = do
  putStrLn "Parsing input..."
  contents <- readFile "input"
  let reports = map ( map readInt ) $ map words (lines contents)
  
  putStrLn "Running solver..."  
  
  putStrLn "Solution #1, Number of Safe Reports:"
  print $ length ( filter isSafe (map deltas reports) )
  
  putStrLn "Solution #2, Number of Safe Reports with Problem Dampener:"
  let allPermutations = map listDeltas (map selfAndMissing reports)
--  let allPermutations = map ( map deltas ) (map selfAndMissing reports)
  print $ length ( filter areSafe allPermutations )


readInt :: String -> Int
-- Read the input buffer string into a list of integers
readInt = read


diffPair :: [Int] -> Int
-- Return the difference between the first and second Ints in [Int]
diffPair [] = 0
diffPair xs = (xs !! 1) - (xs !! 0)


deltas :: [Int] -> [Int]
-- Calculates the deltas between each index in a list of integers
deltas [] = []
deltas xs = map diffPair (transpose [init xs, tail xs])

listDeltas :: [[Int]] -> [[Int]]
listDeltas [] = []
listDeltas xs = map deltas xs


isSafe :: [Int] -> Bool
-- isSafe logic for the Red-nosed Reactor data, for one list of deltas
isSafe xs = (isSafeInc xs) || (isSafeDec xs)
    where isSafeInc xs = (null $ filter (\x -> x < 1) xs) && (null $ filter (\x -> x > 3) xs)
          isSafeDec xs = (null $ filter (\x -> x < -3) xs) && (null $ filter (\x -> x > -1) xs)

areSafe :: [[Int]] -> Bool
-- With the Problem Dampener, if any permutation is safe, the report is safe
areSafe xs = not $ null $ filter (\x -> x == True) (map isSafe xs)


missingLevels :: [a] -> [[a]]
-- Return each possible report that omits a single level from the original
missingLevels xs = map (missingHelper xs) [1..length xs]
missingHelper lst n = take (n-1) lst ++ drop n lst


selfAndMissing :: [a] -> [[a]]
-- Include the original report among the missing reports
selfAndMissing xs = xs : missingLevels xs