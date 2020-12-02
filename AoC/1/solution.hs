-- runda første advent og nå erre på tide med litt hjernetrim
-- god jul
-- første desember
{-# LANGUAGE TypeApplications #-}

module Main where

import Data.List (find)
import GHC.OldList ((\\))
import System.Exit (exitFailure)

-- Parse the input file to [Int]
nums :: IO [Int]
nums = map (read @Int) . lines <$> readFile "./AoC/1/input.txt"

-- Find the two numbers that sum to `target` and print their product
solve :: Int -> [Int] -> Maybe (Int, Int)
solve target (x : xs) = case result of
  Just x -> return (x, target - x)
  Nothing -> solve target xs
  where
    result = find (\n -> n == target - x) xs
solve _ [] = Nothing

main :: IO ()
main = do
  input <- nums
  -- To solve problem 1.
  -- solve 2020 input

  -- Map with monads and ignore, ditch and desert the result.
  -- Try to find two numbers which sum to 2020 - the current input (reusing old code as we professional code mongers say)
  -- If we find 'em, print the product of 'em. C'est everything
  mapM_
    ( \x -> case solve (2020 - x) (input \\ [x]) of
        Just (a, b) -> print (x * a * b) >> exitFailure -- Let's just crash when we're done. No reason to stick around.
        Nothing -> return ()
    )
    input

-- Remove some of the numbers that are too large. Priming for a brute force.
-- This is merely educated guesswork, and my laptop is running low on power.
--   mapM_ (putStrLn . show) $ filter (\x -> x + (minimum input) * 2 < 2020) input
