module Main where

import Data.List.Split (splitOn)

main :: IO ()
main = do
  chunks <- splitOn "\n\n" <$> readFile "./input.txt"
  let answers = map (countSimilars . lines) chunks
  print $ sum answers

countSimilars :: [String] -> Int
countSimilars = length . foldl1 (\acc curr -> filter (\x -> x `elem` acc && x `elem` curr) acc)