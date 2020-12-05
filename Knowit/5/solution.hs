module Main where

main :: IO ()
main = do
  content <- readFile "./Knowit/5/rute.txt"
  print $ shoelace . scanl f (0, 0) $ content

f :: (Int, Int) -> Char -> (Int, Int)
f (x, y) 'H' = (x + 1, y)
f (x, y) 'V' = (x - 1, y)
f (x, y) 'O' = (x, y + 1)
f (x, y) 'N' = (x, y - 1)

-- Kindly borrowed from the "Rosetta Code" project.
-- https://rosettacode.org/wiki/Shoelace_formula_for_polygonal_area#Haskell
shoelace :: [(Int, Int)] -> Int
shoelace =
  let calcSums ((xi, yi), (nxi, nyi)) (l, r) = (l + xi * nyi, r + nxi * yi)
   in (`div` 2)
        . abs
        . uncurry (-)
        . foldr calcSums (0, 0)
        . (<*>) zip tail