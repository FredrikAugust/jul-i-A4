module Main where

import Data.Text (pack, strip, unpack)

data Cell = Sick | Healthy deriving (Eq)

instance Show Cell where
  show Sick = "S"
  show Healthy = "F"

charToCell :: Char -> Cell
charToCell 'F' = Healthy
charToCell 'S' = Sick

readFileToGrid :: IO [[Cell]]
readFileToGrid = map (map charToCell . unpack . strip . pack) . lines <$> readFile "input.txt"

neighbours :: [[Cell]] -> (Int, Int) -> [Cell]
neighbours cells (x, y) = xneighbours ++ yneighbours
  where
    maxX = (+ (-1)) . length . head $ cells
    maxY = (+ (-1)) . length $ cells
    xneighbours
      | x == 0 = [(cells !! y) !! 1]
      | x == maxX = [(cells !! y) !! (x - 1)]
      | otherwise = [(cells !! y) !! (x + 1), (cells !! y) !! (x - 1)]
    yneighbours
      | y == 0 = [(cells !! 1) !! x]
      | y == maxY = [cells !! (y - 1) !! x]
      | otherwise = [cells !! (y - 1) !! x, cells !! (y + 1) !! x]

nextState :: [[Cell]] -> (Int, Int) -> Cell
nextState cells pos
  | (cells !! snd pos) !! fst pos == Sick = Sick
  | otherwise = case length . filter (== Sick) . neighbours cells $ pos of
    1 -> Healthy
    0 -> Healthy
    _ -> Sick

tick :: [[Cell]] -> [[(Int, Int)]] -> [[Cell]]
tick grid = map (map (nextState grid))

main :: IO ()
main = do
  grid <- readFileToGrid
  -- static
  let positions = [[(x, y) | x <- [0 .. length (head grid) - 1]] | y <- [0 .. length grid - 1]]
  print $ length . takeWhile (\x -> x /= tick x positions) $ iterate (`tick` positions) grid