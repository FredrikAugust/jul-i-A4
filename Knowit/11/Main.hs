module Main where

import Data.Char (chr, ord)
import Data.List (find, isInfixOf, transpose)

-- find position in lowercase alphabet
ord' :: Char -> Int
ord' = (+ (- ord 'a')) . ord

step :: String -> String
step pw = zipWith (\p p' -> chr . (+ ord 'a') $ (ord' p + ord' p') `mod` 26) (init pw) $ map (chr . (+ 1) . ord) $ tail pw

allPossible :: String -> [String]
allPossible pw = transpose . takeWhile (/= "") $ iterate step pw

main :: IO ()
main = do
  ls <- lines <$> readFile "input.txt"
  print $ find (\(_, pws) -> any (isInfixOf "eamqia") pws) $ map (\p -> (p, allPossible p)) ls