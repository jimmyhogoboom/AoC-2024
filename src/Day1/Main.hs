module Day1.Main (part1) where

-- TODO: 
-- 1. Read file
-- 2. Split input
-- 3. Run

day1InputFile = "data/Day1/input.txt"

part1 :: IO String
part1 = do
  contents <- readFile day1InputFile
  return contents
