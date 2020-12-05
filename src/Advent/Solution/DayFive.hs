module Advent.Solution.DayFive (day5) where

import Advent.Problem   (day, Parseable, Day, parseString, both)
import Data.Bits        ((.|.), shiftL)
import Data.List        (foldl', sort, (\\))

newtype SeatID = SeatID { fromSeatID :: Int } deriving (Eq, Ord)

instance Parseable SeatID where
    parseString = SeatID . uncurry ((+) . (8*)) . both (foldl' f 0) . splitAt 7 . map (`elem` "BR")
        where f acc bit = shiftL acc 1 .|. fromEnum bit

day5 :: Day
day5 = day 5 (maximum . map fromSeatID) (missing . sort . map fromSeatID)

missing :: [Int] -> Int
missing ids = head $ [head ids..last ids] \\ ids
