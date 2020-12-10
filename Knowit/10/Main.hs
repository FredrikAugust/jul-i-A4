module Main where

import Data.List (groupBy, maximumBy, sortBy)
import Data.List.Split (splitOn)

scoreForCompetition :: [String] -> [(String, Int)]
scoreForCompetition comp = zip comp $ map (length comp -) [1 ..]

main :: IO ()
main = do
  competitions <- map (splitOn ",") . lines <$> readFile "./input.txt"
  let scores = map (\scores -> (fst $ head scores, sum $ map snd scores)) . groupBy (\x y -> fst x == fst y) . sortBy (\x y -> compare (fst x) (fst y)) $ concatMap scoreForCompetition competitions
  print $ maximumBy (\x y -> compare (snd x) (snd y)) scores