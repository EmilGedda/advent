module Solutions.Y2020.D10 (day10) where

import           Advent.Problem               (Day, day, count)
import           Data.List                    (group)
import           Data.Vector.Algorithms.Intro (sort)
import qualified Data.Vector                  as V

day10 :: Day
day10 = day 10 (partOne . diffs) (partTwo . diffs)

partOne :: V.Vector Int -> Int
partOne = (*) <$> count (==1) <*> count (==3)

diffs :: V.Vector Int -> V.Vector Int
diffs = flip V.snoc 3 . (V.zipWith (-) =<< V.drop 1) . V.cons 0 . V.modify sort

partTwo :: V.Vector Int -> Int
partTwo = product . map (tribonacci . length) . filter (all (==1)) . group . V.toList

-- Tribonacci sequence as a closed form formula only valid for given the input
tribonacci :: Int -> Int
tribonacci n = (n * n - n) `div` 2 + 1
