module Util (countWith, countOf) where

countWith :: (a -> Bool) -> [a] -> Int
countWith p = length . filter p

countOf :: (Eq a) => a -> [a] -> Int
countOf a = countWith (a ==)
