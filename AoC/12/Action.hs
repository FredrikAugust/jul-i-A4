module Action where

data Action
  = North Int
  | South Int
  | East Int
  | West Int
  | Left Int
  | Right Int
  | Forward Int
  deriving (Show)
