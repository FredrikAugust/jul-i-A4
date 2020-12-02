module Main where

import Util (count)

main :: IO ()
main = do
  content <- readFile "./AoC/2/input.txt"
  let ls = lines content
  let ws = map words ls
  let inputs = map (\(r : c : p : _) -> (createRange r, head . takeWhile (/= ':') $ c, p)) ws
  print . length . filter (\(r, c, p) -> inRange r $ count (== c) p) $ inputs

createRange :: String -> [Int]
createRange s = [from .. to]
  where
    from = read . takeWhile (/= '-') $ s
    to = read . drop (1 + length (show from)) $ s

inRange :: [Int] -> Int -> Bool
inRange rn n = n >= head rn && n <= last rn
