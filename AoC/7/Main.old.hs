module Main where

{- Examples of input:
   * vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
   * faded blue bags contain no other bags.
-}

import Data.Either (rights)
import Data.Functor.Identity (Identity)
import Data.List (nub)
import Data.Tree (drawTree, flatten, unfoldTree)
import Text.Parsec (ParsecT)
import Text.ParserCombinators.Parsec
  ( ParseError,
    char,
    digit,
    letter,
    many,
    parse,
    string,
    (<|>),
  )

bag :: ParsecT String () Identity String
bag = do
  adj <- many letter <* char ' '
  colour <- many letter <* char ' ' <* many letter
  return (adj ++ " " ++ colour)

containedBag :: ParsecT String () Identity (String, Int)
containedBag = do
  num <- many digit <* char ' '
  b <- bag
  return (b, read num)

bags :: ParsecT String () Identity (String, [(String, Int)])
bags = do
  b <- bag <* char ' '
  string "contain "
  cb <- many (containedBag <* (string ", " <|> string "."))
  return (b, cb)

getBags :: String -> Either ParseError (String, [(String, Int)])
getBags = parse bags "(source)"

getParentBags :: [(String, [(String, Int)])] -> String -> [String]
getParentBags lbags b = map fst $ filter (\x -> b `elem` (map fst . snd $ x)) lbags

main :: IO ()
main = do
  ls <- lines <$> readFile "./inputTest.txt"
  let res = rights $ map getBags ls
  let tr = unfoldTree (\b -> (b, getParentBags res b)) "shiny gold"
  print $ (+ (-1)) . length . nub . flatten $ tr