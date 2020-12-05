module Advent.Solution.DayFive (day5) where

import Advent.Problem   (day, Parseable, Day, parseString, both)
import Data.List        (foldl1', sort, (\\))

newtype SeatID = SeatID { fromSeatID :: Int } deriving (Eq, Ord)

instance Parseable SeatID where
    parseString = SeatID . uncurry ((+) . (8*)) . both (foldl1' ((+) . (2*)))
                . splitAt 7 . map (fromEnum . (`elem` "BR"))

day5 :: Day
day5 = day 5 (maximum . map fromSeatID) (missing . sort . map fromSeatID)

missing :: [Int] -> Int
missing ids = head $ [head ids..last ids] \\ ids
