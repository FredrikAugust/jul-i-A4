{-# LANGUAGE NamedFieldPuns #-}

module Utils where

import Data.List (nub)
import Lib (Instruction (Jmp, Nop), State (State), accumulator, instructions, pc)

-- |
-- Given an index of an instruction, if it is a jmp/nop. Invert it.
flipI :: Int -> [Instruction] -> [Instruction]
flipI n instructions = case instructions !! n of
  Jmp num -> take n instructions ++ [Nop num] ++ drop (n + 1) instructions
  Nop num -> take n instructions ++ [Jmp num] ++ drop (n + 1) instructions
  _ -> instructions

-- |
-- Generate all unique possible one-flips (ref. flipI). Used on 8th of december
genPossibleOneJmpNopFlips :: State -> [State]
genPossibleOneJmpNopFlips State {instructions, pc, accumulator} =
  map
    ( \instr ->
        State
          { instructions = instr,
            pc = pc,
            accumulator = accumulator
          }
    )
    . nub
    . map (`flipI` instructions)
    $ [0 .. length instructions - 1]