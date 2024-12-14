-- Advent of Code 2024, Day 11
import Data.List

main :: IO ()

main = do
  putStrLn "Parsing input..."
  contents <- readFile "input"
  let stones = map readInt $ words contents
    
  putStrLn "Running solver..."
  putStrLn "Solution #1, Count of Stones after 25 Blinks:"
  let bunchaBlinks = blinkN 25 stones
  print $ length bunchaBlinks
  
  putStrLn "Solution #2, Count of Stones after 75 Blinks:"
  -- List of integers becomes way too long, need to compress the List
  let rnd0 = compress stones
  let rnd1 = consolidate $ distributeList $ compressCompressed $ blinkNCompressed 14 $ transformCompressed rnd0
  let rnd2 = consolidate $ distributeList $ compressCompressed $ blinkNCompressed 14 $ transformCompressed rnd1
  let rnd3 = consolidate $ distributeList $ compressCompressed $ blinkNCompressed 14 $ transformCompressed rnd2
  let rnd4 = consolidate $ distributeList $ compressCompressed $ blinkNCompressed 14 $ transformCompressed rnd3
  let rnd5 = consolidate $ distributeList $ compressCompressed $ blinkNCompressed 14 $ transformCompressed rnd4
  print $ sum $ map snd rnd5

readInt :: String -> Int
readInt = read

splitInt :: Int -> [Int]
-- Split an even-length integer into a left/right half
splitInt n = map readInt sns
   where sn = show n
         len = length sn
         sns = [take (div len 2) sn, drop (div len 2) sn]

transformStone :: Int -> [Int]
-- Apply the first applicable stone transformation rule:
--   If Int == 0, set to [1]
--   If mod 2 $ length $ show == 0, splitInt
--   Else, set to [Int * 2024]
transformStone n = if zero then [1] else if evLen then splitInt n else [n * 2024]
   where zero = n == 0
         evLen = (mod (length $ show n) 2) == 0

blink :: [Int] -> [Int]
blink [] = []
blink xn = (transformStone $ head xn) ++ (blink $ tail xn)

blinkN :: Int -> [Int] -> [Int]
blinkN 0 xn = xn
blinkN _ [] = []
blinkN n xn = blinkN (n-1) (blink xn)

compress :: [Int] -> [(Int,Int)]
compress [] = []
compress xn = (n,cnt) : (compress $ filter (/=n) xn)
   where n   = head xn
         cnt = length $ filter (==n) xn

transformCompressed :: [(Int,Int)] -> [([Int],Int)]
transformCompressed xt = map (\x -> (transformStone (fst x),snd x)) xt

blinkNCompressed :: Int -> [([Int],Int)] -> [([Int],Int)]
blinkNCompressed n xt = map (\x -> (blinkN n (fst x),snd x)) xt

compressCompressed :: [([Int],Int)] -> [([(Int,Int)],Int)]
compressCompressed xt = map (\x -> (compress (fst x),snd x)) xt

distribute :: ([(Int,Int)],Int) -> [(Int,Int)]
distribute tp = map (\x -> (fst x, (snd x)*(snd tp))) (fst tp)

distributeList :: [([(Int,Int)],Int)] -> [(Int,Int)]
distributeList xt = concat $ map distribute xt

consolidate :: [(Int,Int)] -> [(Int,Int)]
consolidate [] = []
consolidate xt = (n,tot) : (consolidate $ filter (\x -> (fst x)/=n) xt)
   where n   = fst $ head xt
         tot = sum $ map (snd) $ filter (\x -> (fst x)==n) xt
