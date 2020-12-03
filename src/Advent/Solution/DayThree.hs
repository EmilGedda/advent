module Advent.Solution.DayThree (day3) where

import Advent.Problem                   (Day, day, Parseable(..), every)
import Data.Vector                      (Vector, fromList, (!))

newtype TreeRow = TreeRow (Vector Bool)

instance Parseable TreeRow where
    parseString = TreeRow . fromList . map ('#' ==)

day3 :: Day
day3 = day 3 (slope 3 1) part2

slope :: Int -> Int -> [TreeRow] -> Int
slope right down = sum . zipWith isTree (every right [0..]) . every down
    where isTree n (TreeRow row) = fromEnum $ row ! (n `mod` length row)

part2 :: [TreeRow] -> Int
part2 grid = product $ map slopes [(1,1), (3,1), (5,1), (7,1), (1,2)]
    where slopes (r,d) = slope r d grid
