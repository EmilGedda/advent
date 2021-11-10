module Solutions.Y2020.D03 (day03) where

import Advent.Problem
import Data.Vector      (Vector, fromList, (!))

newtype TreeRow = TreeRow (Vector Bool) deriving (Generic, NFData)

instance Parseable TreeRow where
    parseString = TreeRow . fromList . map ('#' ==)

day03 :: Day 3
day03 = day (slope 3 1) part2

slope :: Int -> Int -> [TreeRow] -> Int
slope right down = sum . zipWith isTree (every right [0..]) . every down
    where isTree n (TreeRow row) = fromEnum $ row ! (n `mod` length row)

part2 :: [TreeRow] -> Int
part2 grid = product $ map slopes [(1,1), (3,1), (5,1), (7,1), (1,2)]
    where slopes (r,d) = slope r d grid
