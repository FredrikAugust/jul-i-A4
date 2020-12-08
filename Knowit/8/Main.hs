module Main where

-- Santa starts at (0,0)

import Data.List (maximumBy, minimumBy)
import Data.List.Split (splitOn)
import Data.Maybe (fromMaybe)

-- |
-- The name, the position, and the time passed
parseLocations :: [String] -> [((Int, Int), Double)]
parseLocations rLoc = map (\[_, rawPos] -> (read rawPos :: (Int, Int), 0.0)) splitLoc
  where
    splitLoc = map (splitOn ": ") rLoc

-- |
-- Parse the route information from strings to a series of coordinates
parseRoute :: [String] -> [(String, (Int, Int))] -> [(Int, Int)]
parseRoute rRut locs = map (fromMaybe (0, 0) . flip lookup locs) rRut

-- |
-- Parse the locations given to us into a more workable format.
parseMap :: [String] -> [(String, (Int, Int))]
parseMap rLoc = map (\[name, rawPos] -> (name, read rawPos :: (Int, Int))) splitLoc
  where
    splitLoc = map (splitOn ": ") rLoc

-- |
-- A list of the numbers between from and to, incrementing or
-- decremening by 1 based on whether or not the difference between them is positive or negative.
(...) :: Int -> Int -> [Int]
(...) from to
  | from < to = tail [from .. to]
  | from > to = tail [from, from - 1 .. to]
  | otherwise = [from]

-- |
-- Given a starting position, and a list of places to visit, create a list of all coordinates that will be visited.
-- Movement is first completed on the x-axis, and then on the y-axis.
generatePath :: (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
generatePath _ [] = []
generatePath santa (next : route) = trail' ++ generatePath (last trail') route
  where
    xTrail = fst santa ... fst next
    yTrail = snd santa ... snd next
    trail' = zip xTrail (repeat $ snd santa) ++ zip (repeat $ last xTrail) yTrail

-- |
-- The algorithm cab companies use to determine the price for your ride.
taxicabDistance :: (Int, Int) -> (Int, Int) -> Int
taxicabDistance (i, j) (x, y) = abs (i - x) + abs (j - y)

-- |
-- How much time passes given distance d.
timeFromDistance :: Int -> Double
timeFromDistance d
  | d >= 50 = 1.0
  | d >= 20 = 0.75
  | d >= 5 = 0.5
  | d >= 0 = 0.25
  | otherwise = 0

-- |
-- Takes a position of the santa, the position of a location to update, and returns the time-adjusted value.
tick :: (Int, Int) -> ((Int, Int), Double) -> ((Int, Int), Double)
tick pos loc = (fst loc, snd loc + (timeFromDistance . taxicabDistance pos $ fst loc))

-- |
-- This embarks on the final journey.
embark :: [((Int, Int), Double)] -> [(Int, Int)] -> [((Int, Int), Double)]
embark = foldl (\locs pos -> map (tick pos) locs)

main :: IO ()
main = do
  rawLocs <- lines <$> readFile "locations.txt"
  let locations = parseLocations rawLocs
      parsedMap = parseMap rawLocs
  rawRoute <- lines <$> readFile "route.txt"
  let route = parseRoute rawRoute parsedMap
      path = generatePath (0, 0) route
      result = embark locations path
      max' = maximumBy (\x y -> compare (snd x) (snd y)) result
      min' = minimumBy (\x y -> compare (snd x) (snd y)) result
  print $ snd max' - snd min'
