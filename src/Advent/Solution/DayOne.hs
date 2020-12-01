module Advent.Solution.DayOne where

import Advent.Problem               (Day, Input, day, fromInput)
import Control.Monad                (replicateM)
import Data.ByteString.Lazy.Char8   (split, readInt)
import Data.Maybe                   (fromJust)
import Data.List                    (find)

day1 :: Day
day1 = day 1 part1 part2

parse :: Input -> [Int]
parse = map (fst . fromJust . readInt) . split '\n' . fromInput

solveFor :: Int -> Input -> Int
solveFor n = product . fromJust . find ((==) 2020 . sum) . replicateM n . parse

part1 = solveFor 2
part2 = solveFor 3
