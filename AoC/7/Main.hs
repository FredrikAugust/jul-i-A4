module Main where

{- Examples of input:
   * vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
   * faded blue bags contain no other bags.
-}

import Data.Either (rights)
import Data.Functor.Identity (Identity)
import Data.Maybe (fromMaybe)
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

getChildBags :: [(String, [(String, Int)])] -> String -> [(String, Int)]
getChildBags lbags b = fromMaybe [] el
  where
    el = lookup b lbags

-- ("shiny gold",[("dark red",2)])
-- ("dark red",[("dark orange",2)])
-- ("dark orange",[("dark yellow",2)])
-- ("dark yellow",[("dark green",2)])
-- ("dark green",[("dark blue",2)])
-- ("dark blue",[("dark violet",2)])
-- ("dark violet",[])

childSum :: (String, Int) -> [(String, [(String, Int)])] -> Int
childSum ch tr = case children of
  Just [] -> snd ch
  Just children' -> snd ch + snd ch * (sum . map (flip childSum tr) $ children')
  Nothing -> snd ch
  where
    children = lookup (fst ch) tr

main :: IO ()
main = do
  ls <- lines <$> readFile "./input.txt"
  let res = rights $ map getBags ls
  let tr = unfoldTree (\b -> (b, getChildBags res $ fst b)) ("shiny gold", 0)
  mapM_ print res
  mapM_ print tr
  -- -1 for our shiny gold bag
  print $ (+ (-1)) . childSum ("shiny gold", 1) $ res