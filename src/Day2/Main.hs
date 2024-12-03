module Day2.Main (part1) where

import Util (countWith)

day2InputFile :: String
day2InputFile = "data/Day2/input.txt"

day2ExampleFile :: String
day2ExampleFile = "data/Day2/example.txt"

part1 :: IO Int
part1 = do
  contents <- readFile day2InputFile
  let parsed = parseList contents
  let result = countWith id $ map isSafe parsed
  print $ zip (map isSafe parsed) parsed -- debugging
  return result

type Report = [Int]

type Bounds = (Int, Int)

parseReport :: String -> Report
parseReport = map read . words

parseList :: String -> [Report]
parseList s = map parseReport $ lines s

increasing :: Report -> Bool
increasing (a : b : ls)
  | a > b = False
  | otherwise = increasing $ b : ls
increasing _ = True

decreasing :: Report -> Bool
decreasing (a : b : ls)
  | a < b = False
  | otherwise = decreasing $ b : ls
decreasing _ = True

inBounds :: Bounds -> Report -> Bool
inBounds bounds = go
  where
    bot = fst bounds
    top = snd bounds
    go (a : b : rs) =
      let dif = abs $ a - b
       in not (dif < bot || dif > top) && go (b : rs)
    go _ = True

isSafe :: Report -> Bool
isSafe r = (increasing r || decreasing r) && inBounds (1, 3) r
