{-# LANGUAGE OverloadedStrings #-}
module Solutions.Y2021.D05 where

import Advent.Problem
import Data.Attoparsec.ByteString.Char8 hiding (count, take)
import Data.List

data Vent = Vent Coord Coord deriving (Generic, NFData)

vent :: Parser Vent
vent = Vent <$> coord <* " -> " <*> coord
    where coord = Coord <$> decimal <* "," <*> decimal

instance Parseable Vent where
    parseInput = attoparse vent

day05 :: Day 5
day05 = day (overlapping . filter (not . isDiagonal)) overlapping

isDiagonal :: Vent -> Bool
isDiagonal (Vent (Coord x y) (Coord x' y')) = x /= x' && y /= y'

overlapping :: [Vent] -> Int
overlapping = count (not . null . tail) . group . sort . concatMap covering

covering :: Vent -> [Coord]
covering (Vent a b) = take (n + 1) $ iterate (+ signum diff) a
    where n = combineCoord max $ abs diff
          diff = b - a
