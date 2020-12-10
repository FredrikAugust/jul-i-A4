{-# LANGUAGE TypeApplications #-}

module Main where

import Data.List (sort)
import Data.Maybe (fromMaybe)

solve :: [(Int, [Int])] -> [(Int, Int)] -> [(Int, Int)]
solve [(0, [])] memo = (0, 1) : memo -- 0 will always be the start
solve ((num, deps) : xs) memo = (num, foldl (\acc curr -> acc + (fromMaybe 0 $ lookup curr memo')) 0 deps) : memo'
  where
    memo' =
      foldl
        ( \memo' depNum -> case lookup depNum memo' of
            Just x -> (num, x) : memo' -- if we've already solved the subproblem, take the memoised solution
            Nothing -> (solve xs memo') ++ memo' -- else, recursively solve the problem. sub-solutions will propagate upwards
        )
        memo
        deps

main :: IO ()
main = do
  jolts <- sort . map (read @Int) . lines <$> readFile "input.txt"
  let jolts' = 0 : jolts ++ [maximum jolts + 3] -- add extra constraints from exercise
      dependencyGraph = reverse $ map (\curr -> (curr, filter (\x -> curr - x <= 3 && x < curr) jolts')) jolts' -- what jolts can jump to the current
  print $ head $ solve dependencyGraph []