module Day1.Main (part1, part2) where

import Data.List

day1InputFile :: String
day1InputFile = "data/Day1/input.txt"

day1ExampleFile :: String
day1ExampleFile = "data/Day1/example.txt"

part1 :: IO Int
part1 = do
  contents <- readFile day1InputFile
  -- contents <- readFile day1ExampleFile
  let result = calculateTotal $ parseLists contents
  -- print $ result
  return result

parseLists :: String -> ([Int], [Int])
parseLists xs = unzip $ map pairSplit (lines xs)
  where
    pairSplit x = tuplify (words x)
    tuplify [x, y] = (read x, read y)
    tuplify _ = (0, 0)

distance :: Int -> Int -> Int
distance x y = abs $ x - y

distances :: [Int] -> [Int] -> [Int]
distances xs ys = zipWith distance xs' ys'
  where
    xs' = sort xs
    ys' = sort ys

calculateTotal :: ([Int], [Int]) -> Int
calculateTotal = sum . uncurry distances

----

part2 :: IO Int
part2 = do
  contents <- readFile day1InputFile
  let result = similarity $ parseLists contents
  print result
  return result

countWith :: (a -> Bool) -> [a] -> Int
countWith p = length . filter p

countOf :: (Eq a) => a -> [a] -> Int
countOf a = countWith (a ==)

similarity :: ([Int], [Int]) -> Int
similarity (xs, ys) = foldr simScore 0 xs
  where
    simScore a b = b + (a * countOf a ys)
