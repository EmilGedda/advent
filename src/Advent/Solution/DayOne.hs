module Advent.Solution.DayOne where

import Advent.Problem               (Day, Input, day, fromInput)
import Control.Monad                (msum)
import Data.ByteString.Lazy.Char8   (split, readInt)
import Data.Maybe                   (fromJust)

day1 :: Day
day1 = day 1 (part1 . parse) (part2 . parse)

parse :: Input -> [Int]
parse = map (fst . fromJust . readInt) . split '\n' . fromInput

answer :: [Int] -> Maybe Int
answer xs | sum xs == 2020 = Just (product xs)
          | otherwise = Nothing

solve :: [[Int]] -> Int
solve = fromJust . msum . map answer . sequence

part1 input = solve [input, input]
part2 input = solve [input, input, input]
