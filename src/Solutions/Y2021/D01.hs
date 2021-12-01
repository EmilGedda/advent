module Solutions.Y2021.D01 (day01) where

import Advent.Problem

day01 :: Day 1
day01 = day increasing (increasing . window)

increasing :: [Int] -> Int
increasing xs = count id $ zipWith (<) xs (tail xs)

window :: [Int] -> [Int]
window xs = zipWith3 (\a b c -> a + b + c) xs (tail xs) (tail $ tail xs)
