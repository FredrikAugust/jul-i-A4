{-# LANGUAGE NamedFieldPuns #-}

module Main where

import qualified Action as A

data Direction = North | South | East | West deriving (Show)

parseAction :: Char -> Int -> A.Action
parseAction 'N' = A.North
parseAction 'S' = A.South
parseAction 'E' = A.East
parseAction 'W' = A.West
parseAction 'L' = A.Left
parseAction 'R' = A.Right
parseAction 'F' = A.Forward
parseAction _ = error "Invalid action"

parseLine :: String -> A.Action
parseLine (action : value) = parseAction action $ read value

data Position = Position {north :: Int, east :: Int} deriving (Show)

data State = State {direction :: Direction, position :: Position, waypoint :: Position} deriving (Show)

next :: Direction -> Direction
next North = East
next East = South
next South = West
next West = North

prev :: Direction -> Direction
prev = last . take 4 . iterate next

rotate :: Position -> A.Action -> Position
rotate pos@Position {north, east} (A.Right value)
  | value <= 0 = pos
  | otherwise = rotate (Position {east = north, north = - east}) (A.Right (value - 90))
rotate pos@Position {north, east} (A.Left value)
  | value <= 0 = pos
  | otherwise = rotate (Position {north = east, east = - north}) (A.Left (value - 90))

instance Num Position where
  Position {north = north1, east = east1} + Position {north = north2, east = east2} = Position {north = north1 + north2, east = east1 + east2}

tick :: State -> A.Action -> State
tick s@State {waypoint} (A.North value) = s {waypoint = waypoint {north = north waypoint + value}}
tick s@State {waypoint} (A.South value) = s {waypoint = waypoint {north = north waypoint - value}}
tick s@State {waypoint} (A.East value) = s {waypoint = waypoint {east = east waypoint + value}}
tick s@State {waypoint} (A.West value) = s {waypoint = waypoint {east = east waypoint - value}}
tick s@State {waypoint, position} (A.Forward value)
  | value <= 0 = s
  | otherwise = tick (s {position = position + waypoint}) (A.Forward (value - 1))
tick s@State {waypoint} a = s {waypoint = rotate waypoint a}

input :: IO [String]
input = lines <$> readFile "input.txt"

-- input =
--   return $
--     lines
--       "F10\n\
--       \N3\n\
--       \F7\n\
--       \R90\n\
--       \F11"

main :: IO ()
main =
  ( print
      . foldl
        tick
        State
          { direction = East,
            position = Position {north = 0, east = 0},
            waypoint = Position {north = 1, east = 10}
          }
      . map parseLine
  )
    =<< input