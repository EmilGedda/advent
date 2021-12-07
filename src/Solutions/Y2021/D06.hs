module Solutions.Y2021.D06 where

import Advent.Problem
import Data.List

day06 :: Day 6
day06 = day (simulate 80) (simulate 256)

simulate :: Int -> CommaList Int -> Int
simulate n = sum . map process . group . sort . getList
    where process xs = length xs * lantern !! (n + 6 - head xs)

lantern :: [Int]
lantern = replicate 7 1 ++ zipWith (+) lantern (1:1:lantern)
