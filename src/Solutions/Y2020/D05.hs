module Solutions.Y2020.D05 (day05) where

import Advent.Problem
import Data.List        (foldl1', sort, (\\))

newtype SeatID = SeatID { fromSeatID :: Int } deriving (Eq, Ord, Generic, NFData)

instance Parseable SeatID where
    parseString = SeatID . uncurry ((+) . (8*)) . both (foldl1' ((+) . (2*)))
                . splitAt 7 . map (fromEnum . (`elem` "BR"))

day05 :: Day 5
day05 = day (maximum . map fromSeatID) (missing . sort . map fromSeatID)

missing :: [Int] -> Int
missing ids = head $ [head ids..last ids] \\ ids
