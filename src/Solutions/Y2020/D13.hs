{-# LANGUAGE OverloadedStrings #-}
module Solutions.Y2020.D13 (day13) where

import Advent.Problem
import Control.Applicative              ((<|>))
import Control.Arrow                    ((&&&))
import Data.Attoparsec.ByteString.Char8
import Data.Functor

data Puzzle = Puzzle {
                    timestamp :: Int,
                    buses :: [Int]
                } deriving (Show, Generic, NFData)

puzzle = Puzzle <$> decimal <* "\n" <*> (decimal <|> "x" $> 0) `sepBy` ","

instance Parseable Puzzle where
    parseInput = fromRight . parseOnly puzzle

day13 :: Day 13
day13 = day partOne' partTwo'

partOne' :: Puzzle -> Int
partOne' (Puzzle timestamp buses)
  = uncurry (*) . minimum
  . map ((-) <*> mod timestamp &&& id)
  $ filter (>0) buses

partTwo' :: Puzzle -> Int
partTwo' (Puzzle _ buses)
  = (crt =<< product . map snd)
  . filter ((>0) . snd) $ zip [0..] buses

euclidean :: Int -> Int -> Int
euclidean a b = flip mod b . fst $ go a b
    where go _ 0 = (1, 0)
          go a b =
            let (q, r) = quotRem a b
                (s, t) = go b r
            in (t, s - q * t)

crt :: Int -> [(Int, Int)] -> Int
crt n = smallest n . sum . map linear
    where linear (ai, ni) = let x = div n ni in ai * x * euclidean x ni
          smallest n res = n - res `mod` n

