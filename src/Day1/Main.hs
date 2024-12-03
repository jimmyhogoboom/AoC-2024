module Day1.Main (part1) where

import Data.List

day1InputFile :: String
day1InputFile = "data/Day1/input.txt"

day1ExampleFile :: String
day1ExampleFile = "data/Day1/example.txt"

part1 :: IO Integer
part1 = do
  contents <- readFile day1InputFile
  -- contents <- readFile day1ExampleFile
  let result = calculateTotal $ parseLists contents
  -- print $ result
  return result

parseLists :: String -> ([Integer], [Integer])
parseLists xs = unzip $ map pairSplit (lines xs)
  where
    pairSplit x = tuplify (words x)
    tuplify [x, y] = (read x, read y)
    tuplify _ = (0, 0)

distance :: Integer -> Integer -> Integer
distance x y = abs $ x - y

distances :: [Integer] -> [Integer] -> [Integer]
distances xs ys = zipWith distance xs' ys'
  where
    xs' = sort xs
    ys' = sort ys

calculateTotal :: ([Integer], [Integer]) -> Integer
calculateTotal = sum . uncurry distances
