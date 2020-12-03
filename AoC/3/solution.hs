module Main where

step :: Int -> Int -> Int -> [String] -> Int
step _ _ encounters [] = encounters
step interval stepsRight encounters (level : _ : levels) = step interval (stepsRight' + interval) (if level !! stepsRight' == '#' then encounters + 1 else encounters) levels
  where
    stepsRight' = stepsRight `mod` length level

main :: IO ()
main = do
  ls <- lines <$> readFile "./AoC/3/input.txt"
  print $ step 1 1 0 (tail . tail $ ls)

-- Right/Down
-- 1/1: 88
-- 3/1: 145
-- 5/1: 71
-- 7/1: 90
-- 1/2: 42
-- product = 3342992400 WRONG