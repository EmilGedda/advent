module Advent.Solution.DayFifteen where

import Advent.Problem                   (Day, notSolved, day, Parseable(..), parseInput, )
import Data.ByteString.Char8            (split)
import qualified Data.Map as M

newtype Numbers = Numbers { getNumbers :: [Int] }

instance Parseable Numbers where
    parseInput = Numbers . map parseInput . split ','

day15 :: Day
day15 = day 15 ((!! 2019) . speak) ((!! 29999999) . speak)


speak n@(Numbers xs) =
    let start = zip xs [1..]
    in xs ++ go (M.fromList start) (length start + 1) (fst $ last start)
    where go m i v
            | M.member v m = let val = i - 1 - (m M.! v)
                             in val:go (M.insert v (i - 1) m) (i + 1) val
            | otherwise = 0:go (M.insert v (i - 1) m) (i + 1) 0
