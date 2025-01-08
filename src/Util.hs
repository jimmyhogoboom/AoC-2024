module Util (countWith, countOf, deleteAt) where

countWith :: (a -> Bool) -> [a] -> Int
countWith p = length . filter p

countOf :: (Eq a) => a -> [a] -> Int
countOf a = countWith (a ==)

deleteAt :: Int -> [a] -> [a]
deleteAt i xs = take i xs ++ drop (i + 1) xs
