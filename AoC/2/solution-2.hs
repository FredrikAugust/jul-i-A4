-- Second challenge
module Main where

import Util (count)

main :: IO ()
main = do
  content <- readFile $ "./AoC/2/input.txt"
  let ls = lines content
  let ws = map (words) ls
  -- We have three values; the two positions, the character, and the password
  let inputs = map (\(r : c : p : _) -> (positions r, head . takeWhile (/= ':') $ c, p)) ws
  -- Map the positions to the underlying characters
  -- Count how many of these two characters are equal to our target character
  -- Make sure this is exactly one
  -- Filter all entries with this and count up how many passed
  print . length . filter (\(r, c, p) -> (== 1) . count (== c) . map (p !!) $ r) $ inputs

-- Derive the two indices from the input (e.g. "12-35") string
positions :: String -> [Int]
positions s = [from - 1, to - 1] -- convert to 0-index
  where
    from = read . takeWhile (/= '-') $ s
    to = read . drop (1 + length (show from)) $ s
