{-# LANGUAGE TypeApplications #-}

module Main where

import Data.List (sort)

main :: IO ()
main = do
  content <- readFile "./Knowit/1/numbers.txt"
  let strNumbers = words [if c == ',' then ' ' else c | c <- content]
  let numbers = sort . map (read @Int) $ strNumbers
  f numbers

f :: (Eq a, Num a, Show a) => [a] -> IO ()
f (a : b : t)
  | a + 1 == b = f (b : t)
  | otherwise = print [a, b] >> f (b : t)
f [_] = print "finito"
