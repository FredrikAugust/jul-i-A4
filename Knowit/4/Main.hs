module Main where

import Data.List.Split (splitOn)

separateIngredients :: String -> [[String]]
separateIngredients = map (splitOn ": ") . splitOn ", "

cleanIngredients :: [[String]] -> [(String, Int)]
cleanIngredients = map (\[x, y] -> (x, read y))

main :: IO ()
main = do
  ls <- lines <$> readFile "input.txt"
  let shipments = concatMap (cleanIngredients . separateIngredients) ls
      sukker = foldl (\sum' (_, count) -> sum' + count) 0 . filter ((== "sukker") . fst) $ shipments
      mel = foldl (\sum' (_, count) -> sum' + count) 0 . filter ((== "mel") . fst) $ shipments
      melk = foldl (\sum' (_, count) -> sum' + count) 0 . filter ((== "melk") . fst) $ shipments
      egg = foldl (\sum' (_, count) -> sum' + count) 0 . filter ((== "egg") . fst) $ shipments
  print $ minimum [sukker `div` 2, mel `div` 3, melk `div` 3, egg]