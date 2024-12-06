-- Advent of Code 2024, Day 5
import Data.List

main :: IO ()

main = do
  putStrLn "Parsing input..."
  contents <- readFile "input"
  let inputs = words contents
  let rules = map rulesToInt $ filter ("|" `isSubsequenceOf`) inputs
  let updates = map updatesToInt $ filter ("," `isSubsequenceOf`) inputs
  
  putStrLn "Running solver..."
  let orderedUpdates = filter (isOrdered rules) updates
  putStrLn "Solution #1, Sum of Middle Numbers of Correctly-Ordered Updates:"
  print $ sum $ map middle orderedUpdates
  
  let unorderedUpdates = filter (not . isOrdered rules) updates
  let reorderedUpdates = map (fixUpdate rules) unorderedUpdates
  putStrLn "Solution #2, Sum of Middle Numbers of Reordered Updates:"
  print $ sum $ map middle reorderedUpdates


readInt :: String -> Int
-- Convert the input string to an Int
readInt = read

rulesToInt :: String -> [Int]
-- Parse a pair of integer strings "XXX|YYY" into an Int list [XXX,YYY]
rulesToInt "" = []
rulesToInt rul = map readInt xs
    where xs = [takeWhile (/='|') rul, drop 1 (dropWhile (/='|') rul)]

updatesToInt :: String -> [Int]
-- Parse a comma-separated string of integers into an Int list
updatesToInt "" = []
updatesToInt upd = readInt (takeWhile (/=',') upd) : updatesToInt (drop 1 dw)
    where dw = dropWhile (/=',') upd

middle :: [a] -> a
-- From any non-null list, return the middle (or middle-left) element
middle xs = xs !! m
    where m = (length xs) `div` 2

obeysRule :: [Int] -> [Int] -> Bool
-- For a single rule list, return True if the update list obeys it (or it doesn't apply)
obeysRule rul upd = (rul `isSubsequenceOf` upd) || missingA || missingB
    where missingA = null $ filter (==(rul !! 0)) upd
          missingB = null $ filter (==(rul !! 1)) upd

isOrdered :: [[Int]] -> [Int] -> Bool
-- Perform the above for a list of rules
isOrdered [] _ = True
isOrdered _ [] = True
isOrdered ruls upd = b && isOrdered (drop 1 ruls) upd
    where b = obeysRule (head ruls) upd

swap :: [Int] -> [Int] -> [Int]
-- Swap a pair of elements that exist in a list with their partner
-- swap "ab" "banana" ==> "abnbnb"
swap pair xs = map (\case { p0 -> p1; p1 -> p0; _ -> x }) xs
    where p0 = pair !! 0
          p1 = pair !! 1

applyRule :: [Int] -> [Int] -> [Int]
-- If a rule is violated, swap the elements that apply 
--    (doesn't always work the first time, but always works eventually)
applyRule rul upd = if (obeysRule rul upd) then upd else swapped
    where swapped = swap rul upd

applyRules :: [[Int]] -> [Int] -> [Int]
-- Perform the above for every rule in a list of rules
applyRules []   upd = upd
applyRules _    []  = []
applyRules ruls upd = applyRules (drop 1 ruls) (applyRule (head ruls) upd)

fixUpdate :: [[Int]] -> [Int] -> [Int]
-- Cycle through the rules to produce an update that isOrdered from one that isn't
fixUpdate _    []  = []
fixUpdate ruls upd = if (isOrdered ruls upd) then upd else fixUpdate ruls (applyRules ruls upd)
