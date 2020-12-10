{-# LANGUAGE TypeApplications #-}

module Main where

import Data.List (sort)

diffs :: Int -> [Int] -> [Int]
diffs _ [] = [3]
diffs prev (jolt : jolts) = (jolt - prev) : diffs jolt jolts

main :: IO ()
main = do
  jolts <- sort . map (read @Int) . lines <$> readFile "input.txt"
  let diffs' = diffs 0 jolts
      ones = length . filter (== 1) $ diffs'
      threes = length . filter (== 3) $ diffs'
  print $ ones * threes