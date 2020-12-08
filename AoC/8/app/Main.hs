{-# LANGUAGE LambdaCase #-}

module Main where

import Lib
  ( State (State, accumulator, instructions, pc),
    Terminated (Finished, Loop),
    parseFile,
    runUntilCompletion,
  )
import Utils (genPossibleOneJmpNopFlips)

main :: IO ()
main = do
  programInstructions <- parseFile "./app/input.txt"
  print $ -- Print it
    filter
      ( \case
          Loop _ -> False
          Finished _ -> True -- Take only the ones that didn't loop
      )
      . map runUntilCompletion -- Run them until they terminate or encounter a loop
      . genPossibleOneJmpNopFlips -- Generate all possible one-flips
      $ State {instructions = programInstructions, pc = 0, accumulator = 0} -- Take initial state
