import Lib
import Test.HUnit (Counts, Test (..), assertEqual, runTestTT)

main :: IO Counts
main =
  runTestTT $
    TestList
      [ TestLabel "nop" parseNop,
        TestLabel "acc" parseAcc,
        TestLabel "jmp" parseJmp,
        TestLabel "junk" parseJunk,
        TestLabel "tick acc" performAcc,
        TestLabel "tick jmp" performJmp
      ]

parseNop :: Test
parseNop = TestCase (assertEqual "parse nop" (Nop 0) (lineToInstruction "nop +0"))

parseAcc :: Test
parseAcc = TestCase (assertEqual "parse acc" (Acc 12) (lineToInstruction "acc +12"))

parseJmp :: Test
parseJmp = TestCase (assertEqual "parse acc" (Jmp (-99)) (lineToInstruction "jmp -99"))

parseJunk :: Test
parseJunk = TestCase (assertEqual "parse junk" Unknown (lineToInstruction "shr -99"))

performAcc :: Test
performAcc =
  TestCase
    ( assertEqual
        "perform acc"
        State
          { pc = 1,
            instructions = [Acc 12],
            accumulator = 12
          }
        (tick (State {pc = 0, instructions = [Acc 12], accumulator = 0}))
    )

performJmp :: Test
performJmp =
  TestCase
    ( assertEqual
        "perform jmp"
        State
          { pc = 1,
            instructions = [Nop 0, Nop 0, Nop 0, Nop 0, Jmp (-3)],
            accumulator = 0
          }
        (tick (State {pc = 4, instructions = [Nop 0, Nop 0, Nop 0, Nop 0, Jmp (-3)], accumulator = 0}))
    )