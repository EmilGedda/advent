module Advent.Solution.DayTwo where

import Prelude hiding               (drop)
import Advent.Problem               (Day, day, Parseable(..))
import Data.ByteString.Lazy.Char8   (readInt, drop, unpack, uncons)
import Control.Lens                 ((^?), element)

data Policy = Policy Int Int Char String

instance {-# OVERLAPPING #-} Parseable Policy where
    parse input =
        let Just (lowerBound, rest) = readInt input
            Just (upperBound, rest') = readInt $ drop 1 rest
            Just (char, rest'') = uncons $ drop 1 rest'
            password = unpack $ drop 2 rest''
        in Policy lowerBound upperBound char password

day2 :: Day
day2 = day 2 (length . filter valid1) (length . filter valid2)

valid1 :: Policy -> Bool
valid1 (Policy lower upper char password) = lower <= occurs && occurs <= upper
    where occurs = length $ filter (char ==) password

valid2 :: Policy -> Bool
valid2 (Policy lower upper char password) = correct lower /= correct upper
    where correct at = Just True == ((== char) <$> password ^? element (at - 1))
