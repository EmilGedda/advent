module Solutions.Y2021.D03 (day03) where

import Advent.Problem
import Data.List
import Data.Char
import Data.Bits

day03 :: Day 3
day03 = day (solve byColumn) (solve byFilter)

solve :: (([Int] -> Int) -> [[Int]] -> [Int]) -> [String] -> Int
solve f = ((*) <$> rule id <*> rule (xor 1)) . map (map digitToInt)
    where rule a = fromBits . f (a . whichBit)

byColumn :: ([Int] -> Int) -> [[Int]] -> [Int]
byColumn selector = map selector . transpose

byFilter :: ([Int] -> Int) -> [[Int]] -> [Int]
byFilter _ [n] = n
byFilter selector ints = bit : byFilter selector [ ys | y:ys <- ints, bit == y]
    where bit = selector $ map head ints

whichBit :: [Int] -> Int
whichBit = fromEnum
         . (>=0)
         . sum
         . map (pred . (*2))
