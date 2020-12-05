module Main where

import Data.List (sort, (\\))

main :: IO ()
main = do
  dirs <- lines <$> readFile "./AoC/5/input.txt"
  let ids = map getID dirs
  print $ [minimum ids .. maximum ids] \\ sort ids

getID :: String -> Int
getID dir = getRow dir * 8 + getCol dir

getRow :: String -> Int
getRow dir = head . foldl (\acc f' -> f' acc) [0 .. 127] . map getRowColF $ rowDir
  where
    rowDir = take 7 dir

getCol :: String -> Int
getCol dir = head . foldl (\acc f' -> f' acc) [0 .. 7] . map getRowColF $ rowDir
  where
    rowDir = drop 7 dir

getRowColF :: Char -> ([Int] -> [Int])
getRowColF 'B' = takeLastHalf
getRowColF 'F' = takeFirstHalf
getRowColF 'R' = takeLastHalf
getRowColF 'L' = takeFirstHalf

takeFirstHalf :: [Int] -> [Int]
takeFirstHalf rng = take (length rng `div` 2) rng

takeLastHalf :: [Int] -> [Int]
takeLastHalf rng = drop (length rng `div` 2) rng