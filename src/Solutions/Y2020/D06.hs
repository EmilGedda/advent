module Solutions.Y2020.D06 (day06) where

import Advent.Problem               (Parseable, day, Day, parseString)
import Data.Containers.ListUtils    (nubOrd)
import Data.List                    (intersect)
import Data.List.Split              (splitOn)

newtype Answers = Answers { fromAnswers :: [[String]] }

instance Parseable Answers where
    parseString = Answers . map words . splitOn "\n\n"

day06 :: Day 6
day06 = day (collect $ nubOrd . concat) (collect $ foldl1 intersect)

collect :: Foldable t => ([String] -> t a) -> Answers -> Int
collect f = sum . map (length . f) . fromAnswers
