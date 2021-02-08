module Solutions.Y2020.D09 (day09) where

import           Advent.Problem (Day, day)
import qualified Data.Vector    as V

day09 :: Day 9
day09 = day partOne partTwo

partOne :: V.Vector Integer -> Integer
partOne input
    | invalid (V.take 25 input) (input V.! 25) = input V.! 25
    | otherwise = partOne (V.drop 1 input)
    where invalid s n = all (\x -> (n - x) `V.notElem` s) s

partTwo :: V.Vector Integer -> Integer
partTwo input = go input 0 0
    where ans = partOne input
          go input len sum
            | sum == ans = (+) <$> minimum <*> maximum $ V.take len input
            | sum < ans  = go input            (len+1) $ sum + input V.! len
            | sum > ans  = go (V.drop 1 input) (len-1) $ sum - V.head input
