{-# LANGUAGE TypeApplications #-}

module Main where

import Data.Maybe (fromMaybe)

possible2Sums :: [Int] -> [Int]
possible2Sums nums = map (\(x, y) -> (nums !! x) + (nums !! y)) indices
  where
    indices = filter (uncurry (/=)) [(x, y) | x <- [0 .. length nums - 1], y <- [0 .. length nums - 1]]

findContiguousSum :: [Int] -> Int -> Maybe [Int]
findContiguousSum nums target = if last chain == target then Just (take (length chain) nums) else Nothing
  where
    chain = takeWhile (<= target) . scanl1 (+) $ nums

main :: IO ()
main = do
  nums <- map (read @Int) . lines <$> readFile "./input.txt"
  -- let checkIndices = [25 .. length nums - 1]
  -- print $ fst . head . filter ((== False) . snd) . map (\i -> (nums !! i, (nums !! i) `elem` possible2Sums (take 25 . drop (i - 25) $ nums))) $ checkIndices
  let result = fromMaybe [] . head . take 1 . dropWhile (== Nothing) . map (\i -> findContiguousSum (drop i nums) 104054607) $ [0 .. length nums - 1]
  print $ maximum result + minimum result
