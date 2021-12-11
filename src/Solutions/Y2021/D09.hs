module Solutions.Y2021.D09 (day09, minima) where
import Advent.Problem
import Data.List
import Data.Function

day09 :: Day 9
day09 = day lowpoints notSolved

lowpoints :: [Digits] -> Int
lowpoints input = sum
            . map (succ . fst)
            . filter snd
            . mconcat
            . zipWith zip grid
            $ zipWith (zipWith ((&&) `on` snd)) rows cols
    where grid = map digits input
          rows = map (zip <*> minima) grid
          cols = transpose . map (zip <*> minima) $ transpose grid

minima :: [Int] -> [Bool]
minima xs = zipWith3 (\a b c -> a > b && b < c) (10:xs) xs (tail xs ++ [10])
