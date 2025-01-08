module Day2.Main where

import Util (countWith, deleteAt)

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

---

-- TODO: Should be true: [43,41,43,44,45,47,49]
-- Removing the first item makes it safe

part2 :: IO Int
part2 = do
  contents <- readFile day2InputFile
  -- contents <- readFile day2ExampleFile
  let parsed = parseList contents
  -- let result = map (isSafeWithTolerance 1) parsed
  -- print $ zip result parsed
  -- return $ countWith id result
  return 0

-- TODO: If a fault is found, try every permutation of the Report with a value missing, both tests

type FaultTolerance = Int

inBounds' :: Bounds -> Int -> Int -> Bool
inBounds' (bot, top) x y = dif >= bot && dif <= top
  where
    dif = abs $ x - y

safelyIncreasing :: Bounds -> Int -> Int -> Bool
safelyIncreasing b x y = x < y && inBounds' b x y

safelyDecreasing :: Bounds -> Int -> Int -> Bool
safelyDecreasing b x y = x > y && inBounds' b x y

-- increasingWithTolerance :: Bounds -> FaultTolerance -> Report -> Bool

-- if not increasing, try with each value missing until it is
-- might have to do a second layer of recursion to test all those

-- type FaultCount = Int

-- data FaultTolerantResult = FaultTolerantResult
--   { safe :: Bool,
--     verifiedReport :: Report,
--     faultCount :: Int
--   }
--   deriving (Show)

-- increasingWithTolerance :: FaultTolerance -> Report -> FaultTolerantResult
-- increasingWithTolerance t r = go r r 0 0
--   where
--     go :: Report -> Report -> Int -> FaultCount -> FaultTolerantResult
--     go r' (a : b : ls) i faults
--       | a > b && faults >= t = FaultTolerantResult {safe = False, verifiedReport = r', faultCount = faults + 1} -- If this is another fault and faults are already at the limit
--       | a > b =
--           let newReportB = deleteAt (i + 1) r
--               newReportA = deleteAt i r
--               resultA = go newReportA newReportA 0 (faults + 1)
--               resultB = go newReportB newReportB 0 (faults + 1)
--            in if safe resultA then resultA else if safe resultB then resultB else FaultTolerantResult {safe = False} -- Try again without the faulty value
--       | otherwise = go r' (b : ls) (i + 1) faults -- continue to next index
--     go r' _ _ faults = FaultTolerantResult {safe = faults <= t, verifiedReport = r', faultCount = faults}

-- decreasingWithTolerance :: FaultTolerance -> Report -> FaultTolerantResult
-- decreasingWithTolerance t r = go r r 0 0
--   where
--     go :: Report -> Report -> Int -> FaultCount -> FaultTolerantResult
--     go r' (a : b : ls) i faults
--       | a < b && faults >= t = FaultTolerantResult {safe = False, verifiedReport = r', faultCount = faults + 1} -- If this is another fault and faults are already at the limit
--       | a < b =
--           let newReportB = deleteAt (i + 1) r
--               newReportA = deleteAt i r
--               resultA = go newReportA newReportA 0 (faults + 1)
--               resultB = go newReportB newReportB 0 (faults + 1)
--            in if safe resultA then resultA else if safe resultB then resultB else FaultTolerantResult {safe = False} -- Try again without the faulty value
--       | otherwise = go r' (b : ls) (i + 1) faults -- continue to next index
--     go r' _ _ faults = FaultTolerantResult {safe = faults <= t, verifiedReport = r', faultCount = faults}

-- inBoundsWithTolerance :: FaultTolerance -> Bounds -> Report -> FaultCount -> Bool
-- inBoundsWithTolerance t bounds r = go r 0
--   where
--     bot = fst bounds
--     top = snd bounds
--     go (a : b : rs) i faults =
--       let dif = abs $ a - b
--        in if dif < bot || dif > top
--             then faults < t && go (deleteAt (i + 1) r) 0 (faults + 1)
--             else go (b : rs) (i + 1) faults
--     go _ _ faults = faults <= t

-- isSafeWithTolerance :: FaultTolerance -> Report -> Bool
-- isSafeWithTolerance t r =
--   let increasingResult = increasingWithTolerance t r
--       decreasingResult = decreasingWithTolerance t r
--       result
--         | safe increasingResult = increasingResult
--         | safe decreasingResult = decreasingResult
--         | otherwise = FaultTolerantResult {safe = False, verifiedReport = []}
--    in safe result && inBoundsWithTolerance t (1, 3) (verifiedReport result) (faultCount result)
