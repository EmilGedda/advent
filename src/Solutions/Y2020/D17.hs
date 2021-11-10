{-# LANGUAGE BangPatterns #-}
module Solutions.Y2020.D17 (day17) where

import Advent.Problem
import Control.Arrow        ((&&&))
import Control.Monad        (replicateM)
import qualified Data.Set   as S

type Coordinates = [Int]
newtype Grid = Grid (Int, [Bool]) deriving (Generic, NFData)

instance Parseable Grid where
    parseString = Grid . (length . takeWhile (/='\n') &&& map (=='#') . filter (/='\n'))


day17 :: Day 17
day17 = day (solve 3) (solve 4)

solve :: Int -> Grid -> Int
solve n = S.size
        . (!! 6)
        . iterate (generation offsets)
        . index n
    where offsets = filter (not . all (==0)) (replicateM n [-1..1])

index :: Int -> Grid -> S.Set Coordinates
index dimensions (Grid (width, alive))
  = S.fromList . map (coord . fst) . filter snd $ zip [0..] alive
    where coord i = let (y, x) = i `divMod` width in x:y:replicate (dimensions - 2) 0

generation :: [Coordinates] -> S.Set Coordinates -> S.Set Coordinates
generation offsets s = S.filter (active s offsets) . S.unions
                     $ S.map (S.fromDistinctAscList . neighbors) s
    where neighbors origin = map (zipWith (+) origin) offsets

active :: S.Set Coordinates -> [Coordinates] -> Coordinates -> Bool
active s offsets origin =
    let actives = length . filter (`S.member` s) $ map (zipWith (+) origin) offsets
    in S.member origin s && actives == 2 || actives == 3
