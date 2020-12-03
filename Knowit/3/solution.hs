module Main where

import Data.List (isInfixOf, transpose)

-- |
-- Get the words to look for
mWords :: IO [String]
mWords = lines <$> readFile "./Knowit/3/wordlist.txt"

-- |
-- Get the words to look for
mMatrix :: IO [String]
mMatrix = lines <$> readFile "./Knowit/3/matrix.txt"

-- |
-- Check if a given word is in a matrix (only horizontal normal direction)
-- This matrix does not need to be an actual matrix, just a list of lists.
wordInMatrix :: String -> [String] -> Bool
wordInMatrix word = any ((== True) . isInfixOf word)

-- |
-- Check horizontal (forwards and reverse)
checkHorizontal :: String -> [String] -> Bool
checkHorizontal word matrix = wordInMatrix word matrix || wordInMatrix word revMx
  where
    revMx = map reverse matrix

-- |
-- Check vertical (up'n'down)
checkVertical :: String -> [String] -> Bool
checkVertical word matrix = checkHorizontal word $ transpose matrix

-- |
-- Check diagonal (all directions)
checkDiagonal :: String -> [String] -> Bool
checkDiagonal word matrix = wordInMatrix word allDiags
  where
    bl = getDiagonals matrix
    tr = getDiagonals . transpose $ matrix
    br = getDiagonals . map reverse $ matrix
    tl = getDiagonals . transpose . map reverse $ matrix
    allDiags' = bl ++ tr ++ br ++ tl
    allDiags = allDiags' ++ map reverse allDiags'

-- |
-- Get the actual diagonal words (only forwards top-left to bottom-right)
getDiagonals :: [String] -> [String]
getDiagonals matrix = map (map (\(x, y) -> (matrix !! y) !! x)) indices
  where
    indices = take (length matrix) . iterate genDiagIterF $ [(0, length matrix - 1)]

-- |
-- The functon that will be used in the "iterate" call to generate the next line of diagonal indices
genDiagIterF :: [(Int, Int)] -> [(Int, Int)]
genDiagIterF indices = scanr1 (\_ (x, y) -> (x - 1, y - 1)) (indices ++ [(lx + 1, ly)])
  where
    (lx, ly) = last indices

-- |
-- Check all!
checkAll :: String -> [String] -> Bool
checkAll word matrix = checkHorizontal word matrix || checkVertical word matrix || checkDiagonal word matrix

main :: IO ()
main = do
  ws <- mWords
  mx <- mMatrix
  mapM_ (\w -> print (w, w `checkAll` mx)) ws