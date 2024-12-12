-- Advent of Code 2024, Day 7
import Data.List

main :: IO ()

main = do
  putStrLn "Parsing input..."
  contents <- readFile "input"
  let eqns = map parseTuple $ map words (lines contents)
  
  putStrLn "Running solver..."
  let ops = [(+),(*)]
  putStrLn "Solution #1, Sum of Valid Calibrations:"
  let cals = filter (not . null) $ map (evaluateTuple ops) eqns
  print $ sum $ map head cals
  
  putStrLn "Solution #2, Sum of Valid Calibrations with CC:"
  let ops2 = [(+),(*),(cc)]
  let cals2 = filter (not . null) $ map (evaluateTuple ops2) eqns
  print $ sum $ map head cals2

readInt :: String -> Int
readInt = read

cc :: Int -> Int -> Int
-- New "concatenation" operator: cc 34 125 = 34125
cc a b = readInt $ (show a) ++ (show b)

parseTuple :: [String] -> (Int,[Int])
-- Parse the leftmost string "32858:" ==> fst: 32858
-- Parse the tail ["42","24","1984"] ==> snd: [42,24,1984]
parseTuple inp = ( testVal , nums )
  where testVal = readInt $ init (head inp)
        nums    = map readInt (tail inp)

applyOps :: [Int] -> [(Int -> Int -> Int)] -> Int
-- Apply a list of N-1 operators to a list of N Ints, without precedence
-- applyOps [1, 2, 3, 4] [(+), (*), (cc)] ==> 94
applyOps ints []  = last ints
applyOps ints ops = (last ops) (applyOps (init ints) (init ops)) (last ints) 

tup2Lst :: (a,a) -> [a]
tup2Lst (a,b) = [a,b]
initOps :: [a] -> [[a]]
initOps ops = map tup2Lst $ concatMap (zip ops) rp
   where numOps = length ops
         rp = transpose $ take numOps $ repeat ops
permOps :: [a] -> [[a]] -> [[a]]
permOps ops perms = concat $ map (zipWith (:) ops) rp
   where numOps = length ops
         rp = transpose $ take numOps $ repeat perms
allNOps :: Int -> [a] -> [[a]]
-- allNOps 3 [(+),(*)] ==> [[(+),(+),(+)],[(*),(+),(+)],...]
allNOps 1 ops = map (\x -> [x]) ops
allNOps 2 ops = initOps ops
allNOps n ops = permOps ops (allNOps (n-1) ops)

evaluateAll :: [Int] -> [(Int -> Int -> Int)] -> [Int]
-- Create a list of all possible evaluations
evaluateAll nums ops = map (applyOps nums) perms
   where perms = allNOps (length(nums) - 1) ops

evaluateTuple :: [(Int -> Int -> Int)] -> (Int, [Int]) -> [Int]
evaluateTuple ops tp = filter (==testVal) (evaluateAll ns ops)
   where ns = snd tp
         testVal = fst tp
