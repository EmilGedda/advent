module Solutions.Y2021.D07 (day07) where

import Advent.Problem
import Data.List

day07 :: Day 7
day07 = day (sum . (move =<< middle) . positions) (last . expensive . positions)

positions :: CommaList Int -> [Int]
positions = sort . getList

summed :: Int -> Int
summed n = n * (n + 1) `div` 2

middle :: [Int] -> Int
middle xs = xs !! (length xs `div` 2)

expensive :: [Int] -> [Int]
expensive xs = map val . takeWhile (\(_, a, b) -> a > b) $ zip3 [1..] go (tail go)
    where go = map (\x -> sum . map summed $ move x xs) [1..]
          val (_, _, a) = a

move :: Int -> [Int] -> [Int]
move sel = map (abs . subtract sel)
