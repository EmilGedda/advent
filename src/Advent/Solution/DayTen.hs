module Advent.Solution.DayTen (day10) where

import           Advent.Problem               (Day, day, count)
import           Control.Arrow                ((&&&))
import           Data.List                    (group)
import qualified Data.Vector                  as V
import           Data.Vector.Algorithms.Intro as A

day10 :: Day
day10 = day 10 (partOne . diffs) (partTwo . diffs)

partOne :: V.Vector Int -> Int
partOne = uncurry (*) . (count (==1) &&& count (==3))

diffs :: V.Vector Int -> V.Vector Int
diffs = flip V.snoc 3 . (V.zipWith (-) =<< V.drop 1) . V.cons 0 . V.modify A.sort

partTwo :: V.Vector Int -> Int
partTwo = product . map (combinations . length) . filter (all (==1)) . group . V.toList

-- Tribonacci sequence specialized to a closed form formula for given the input
combinations :: Int -> Int
combinations n = (n ^ 2 - n) `div` 2 + 1
