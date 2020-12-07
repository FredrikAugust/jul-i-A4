module Main where

import Data.List (transpose)

main :: IO ()
main = do
  -- tranpose so we can work with trees in rows instead
  ls <- transpose . lines <$> readFile "./forest.txt"
  let trees = map (filter (not . all (== ' ')) . transpose) $ getTrees ls
  print $ length . filter (== True) . map isTreeSymmetric $ trees

getTrees :: [String] -> [[String]]
getTrees [] = []
getTrees l = tree : getTrees (drop (length tree + 2) l)
  where
    tree = takeWhile (not . all (== ' ')) l :: [String]

isTreeSymmetric :: [String] -> Bool
isTreeSymmetric = all (\x -> x == reverse x)