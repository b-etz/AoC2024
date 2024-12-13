-- Advent of Code 2024, Day 9
import Data.List
import Data.Maybe

main :: IO ()

main = do
  putStrLn "Parsing input..."
  contents <- readFile "input"
  let diskMap = head $ lines contents
  let blocks = blockMap diskMap 0 True
    
  putStrLn "Running solver..."
  putStrLn "Solution #1, Filesystem Checksum:"
  let compactFS = rearrange blocks
  let checksum = sum $ zipWith (*) [0..] compactFS 
  print $ checksum
  
  putStrLn "Solution #2, Defragmented Filesystem Checksum:"
  let chunks = chunkMap diskMap 0 True
  let rearranged = rearrangeChunks 9999 chunks
  let rearrangedFiles = dropWhileEnd (not . isFileChunk) rearranged
  let rearrangedBlocks = chunksToBlocks rearrangedFiles
  let checksum = sum $ zipWith (*) [0..] rearrangedBlocks
  print $ checksum

readInt :: String -> Int
readInt = read

blockMap :: String -> Int -> Bool -> [Int]
blockMap [] _  _      = []
blockMap dm id isFile = if isFile then blocks ++ (blockMap rem (id+1) False) else blanks ++ (blockMap rem id True)
   where nBlk = readInt $ take 1 dm
         rem  = drop 1 dm
         blanks = take nBlk $ repeat (-1)
         blocks = take nBlk $ repeat id

popValid :: [Int] -> ([Int], Maybe Int)
popValid [] = ([], Nothing)
popValid xs = if (val >= 0) then (rem, Just val) else popValid rem
   where val = last xs
         rem = init xs

rearrange :: [Int] -> [Int]
rearrange [] = []
rearrange bm = left ++ mid ++ (rearrange (drop 1 right))
   where left = takeWhile (>(-1)) bm
         mid  = maybeToList (snd pv)
         rest = dropWhile (>(-1)) bm
         pv = popValid rest
         right = fst pv

chunkMap :: String -> Int -> Bool -> [(Int,Int)]
chunkMap [] _  _      = []
chunkMap dm id isFile = if isFile then block ++ (chunkMap rem (id+1) False) else blank ++ (chunkMap rem id True)
   where nBlk = readInt $ take 1 dm
         rem  = drop 1 dm
         blank = if nBlk > 0 then [(-1, nBlk)] else []
         block = if nBlk > 0 then [(id, nBlk)] else []

isFileChunk :: (Int,Int) -> Bool
isFileChunk tp = (fst tp) >= 0

popFileChunk :: [(Int,Int)] -> ([(Int,Int)], Maybe (Int,Int))
popFileChunk [] = ([], Nothing)
popFileChunk xs = if isFileChunk val then (rem, Just val) else popFileChunk rem
   where val = last xs
         rem = init xs

hasSpace :: Int -> (Int,Int) -> Bool
-- Returns True if a blank chunk has enough space to fit N blocks
hasSpace n chnk = (not $ isFileChunk chnk) && ((snd chnk) >= n)

fillSpace :: (Int,Int) -> (Int,Int) -> [(Int,Int)]
fillSpace blank block = [block] ++ remBlank 
   where nRem = (snd blank) - (snd block)
         remBlank = if (nRem > 0) then [(-1,nRem)] else []

replaceChunk :: [(Int,Int)] -> Int -> [(Int,Int)] -> [(Int,Int)]
replaceChunk newChk idx old = (take idx old) ++ newChk ++ (drop (idx+1) old)

rearrangeChunks :: Int -> [(Int,Int)] -> [(Int,Int)]
rearrangeChunks _  [] = []
rearrangeChunks 0  cm = cm
rearrangeChunks id cm = if isNothing maybeSlot then rearrangeChunks (id-1) cm else rearrangeChunks (id-1) slotted
   where chkIdx = fromJust $ findIndex (\x -> (fst x) == id) cm -- OK because ONLY searches valid id
         chunk = cm !! chkIdx
         chunkSize = snd chunk
         maybeSlot = findIndex (hasSpace chunkSize) (take chkIdx cm) -- Search for any open slot LEFT of idIndex
         slotIdx = fromJust maybeSlot -- Only evaluates if isJust
         migrated = replaceChunk [(-1,chunkSize)] chkIdx cm
         slotted = (replaceChunk (fillSpace (cm !! slotIdx) chunk) slotIdx migrated)

chunksToBlocks :: [(Int,Int)] -> [Int]
-- [(3,7)] ==> [3,3,3,3,3,3,3]
-- [(-1,5)] ==> [0,0,0,0,0]
chunksToBlocks [] = []
chunksToBlocks xc = left ++ (chunksToBlocks $ drop 1 xc)
   where tp = head xc
         len = snd tp
         val = fst tp
         left = if (val >= 0) then take len $ repeat val else take len $ repeat 0



