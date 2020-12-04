module Main where

import Data.Char (isDigit, isHexDigit)
import Data.List (isSuffixOf)
import Data.List.Split (splitOn, splitOneOf)

main :: IO ()
main = do
  chunks <- splitOn "\n\n" <$> readFile "./inputInput.txt"
  let passports = map (map (splitOn ":") . splitOneOf "\n ") chunks
  print $ length . filter (\p -> (length . filter (== True) $ p) == 7) . map (map validate') $ passports

-- 202

validate' :: [String] -> Bool
validate' [key, val] = validate key val
validate' _ = False

validate :: String -> String -> Bool
validate "byr" val
  | read val >= 1920 && read val <= 2002 = True
  | otherwise = False
validate "iyr" val
  | read val >= 2010 && read val <= 2020 = True
  | otherwise = False
validate "eyr" val
  | read val >= 2020 && read val <= 2030 = True
  | otherwise = False
validate "hgt" val
  | cm = height >= 150 && height <= 193
  | inch = height >= 59 && height <= 76
  | otherwise = False
  where
    cm = "cm" `isSuffixOf` val
    inch = "in" `isSuffixOf` val
    height = read (take (length val - 2) val) :: Int
validate "hcl" val = head val == '#' && all ((== True) . isHexDigit) (tail val) && length val == 7
validate "ecl" val = val `elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
validate "pid" val = length val == 9 && all isDigit val
validate _ _ = False