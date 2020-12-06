{-# LANGUAGE TypeApplications #-}

module Main where

import Data.List.Split (splitOn)

main :: IO ()
main = do
  content <- readFile "./godteri.txt"
  let packages = map (read @Int) . splitOn "," $ content
  let cumsum = scanl1 (+) packages
  let distributable = zip cumsum . map (`mod` 127) $ cumsum
  print . (`div` 127) . fst . last . filter ((== 0) . snd) $ distributable