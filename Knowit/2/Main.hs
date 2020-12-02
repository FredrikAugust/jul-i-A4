module Main where

import Math.NumberTheory.Primes (precPrime, unPrime)

main :: IO ()
main = do
  -- Take packages from the infinite stream until we satisfy Norway's lust for julegaver.
  print . length . takeWhile (<= 5433000) . iterate (iterF) $ 0

-- |
-- Does the number contain the digit 7?
containsSeven :: Integer -> Bool
containsSeven = any (== '7') . show

-- |
-- The function used to create the infinite list of package deliveries.
-- We need to recurse in case the next package after skipping also contains 7.
iterF :: Integer -> Integer
iterF n
  | containsSeven (n + 1) = iterF $ (n + 1) + (unPrime . precPrime $ (n + 1))
  | otherwise = n + 1
