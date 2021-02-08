module Solutions.Y2020.D01 (day01) where

import Advent.Problem               (Day, day)
import Control.Monad                (replicateM)
import Data.Maybe                   (fromJust)
import Data.List                    (find)

day01 :: Day 1
day01 = day (solveFor 2) (solveFor 3)

solveFor :: Int -> [Int] -> Int
solveFor n = product . fromJust . find ((==) 2020 . sum) . replicateM n
