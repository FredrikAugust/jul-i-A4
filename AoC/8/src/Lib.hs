{-# LANGUAGE NamedFieldPuns #-}

module Lib where

import Data.List (nub)
import Data.List.Split (splitOn)

-- Data declarations
data Instruction = Nop Int | Acc Int | Jmp Int | Unknown deriving (Show, Eq)

data State = State
  { accumulator :: Int,
    pc :: Int,
    instructions :: [Instruction]
  }
  deriving (Show, Eq)

data Terminated = Loop State | Finished State deriving (Show)

getLines :: String -> IO [String]
getLines file = lines <$> readFile file

argNumToInt :: String -> Int
argNumToInt (sign : num) = case sign of
  '-' -> read (sign : num)
  '+' -> read num
  _ -> 0

lineToInstruction :: String -> Instruction
lineToInstruction rawInstruction = case instr of
  "nop" -> Nop (argNumToInt $ head arguments)
  "acc" -> Acc (argNumToInt $ head arguments)
  "jmp" -> Jmp (argNumToInt $ head arguments)
  _ -> Unknown
  where
    (instr : arguments) = splitOn " " rawInstruction

nextAccumulator :: Int -> Instruction -> Int
nextAccumulator acc (Acc num) = acc + num
nextAccumulator acc _ = acc

nextPc :: Int -> Instruction -> Int
nextPc pc (Jmp num) = pc + num
nextPc pc _ = pc + 1

tick :: State -> State
tick State {accumulator, pc, instructions} =
  State
    { accumulator = nextAccumulator accumulator instruction,
      pc = nextPc pc instruction,
      instructions = instructions
    }
  where
    instruction = instructions !! pc

parseFile :: String -> IO [Instruction]
parseFile file = do
  fileLines <- getLines file
  return $ map lineToInstruction fileLines

memoryTick :: ([Int], State) -> ([Int], State)
memoryTick (history, state) = (pc state : history, tick state)

hasLoop :: Eq a => [a] -> Bool
hasLoop els = length els /= (length . nub $ els)

outOfBounds :: State -> Bool
outOfBounds state = pc state >= (length . instructions $ state)

runUntilCompletion :: State -> Terminated
runUntilCompletion state = if wasLoop then Loop (snd final) else Finished (snd final)
  where
    final =
      memoryTick
        . last
        $ takeWhile
          ( \x ->
              not (outOfBounds $ snd x)
                && not (hasLoop $ fst x) -- check for termination
                -- check for loop
          )
          $ iterate memoryTick ([], state)
    wasLoop = hasLoop $ fst final