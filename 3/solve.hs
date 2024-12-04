-- Advent of Code 2024, Day 3
import Data.List

main :: IO ()

main = do
  putStrLn "Parsing input..." -- Oh, God of Parsing... forgive me, for I have sinned
  contents <- readFile "input"
  
  putStrLn "Running solver..."  
  putStrLn "Solution #1, Sum of Multiples:"
  print $ mulSum contents
  putStrLn "Solution #2, Enabled Sum of Multiples:"
  print $ mulSumEna True contents


isIn :: [Char] -> Char -> Bool
-- Returns True if the second argument appears in the list of chars
isIn [] _ = False
isIn cs c = (c == head cs) || (isIn (tail cs) c)

isNumeric :: String -> Bool
-- Returns True if the String is entirely numeric
isNumeric "" = False
isNumeric str = isIn "0123456789" (head str) && ( null remainder || isNumeric remainder )
    where remainder = tail str

isDo :: String -> Bool
isDo str = ("do()" == take 4 str)

isntDont :: String -> Bool
isntDont str = ("don't()" /= take 7 str)


readInt :: String -> Int
-- Read the input buffer string into a list of integers
readInt = read

leftVal :: String -> Int
-- Returns a numeric value from the head to the first comma, else 0
leftVal "" = 0
leftVal str = if isNumeric s then readInt s else 0
    where s = takeWhile (/=',') str

rightVal :: String -> Int
-- Returns a numeric value from after the first comma to the first close-parens, else 0
rightVal "" = 0
rightVal str = if isNumeric s then readInt s else 0
    where s = takeWhile (/=')') $ drop 1 (dropWhile (/=',') str)

isMul :: String -> Bool
isMul str = ("mul(" == take 4 str)

mulVal :: String -> Int
-- Parse the String to determine its mul(###,###) value
mulVal "" = 0
mulVal str = m * (leftVal s) * (rightVal s)
    where s = drop 4 str
          m = if (isMul str) then 1 else 0

mulSum :: String -> Int
-- Digest the String and add any mulValues within
mulSum "" = 0
mulSum str = mulVal scan + mulSum (drop 1 str)
    where scan = take 12 str

mulSumEna :: Bool -> String -> Int
-- Digest the String and add any mulValues, while updating do/don't status
mulSumEna _     ""  = 0
mulSumEna False str = mulSumEna (isDo scan) (drop 1 str)
    where scan = take 12 str
mulSumEna True  str = mulVal scan + mulSumEna (isntDont scan) (drop 1 str)
    where scan = take 12 str
