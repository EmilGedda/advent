module Advent.Solution where

import Advent.Problem           (Day(..), Input, fetchInput)
import Control.Monad.Except     (runExceptT)
import Data.Maybe               (isNothing, fromJust)
import Data.List                (find)
import Data.Time.Clock          (getCurrentTime, utctDay)
import Data.Time.Calendar       (toGregorian)

import Advent.Solution.DayOne   (day1)
import Advent.Solution.DayTwo   (day2)
import Advent.Solution.DayThree (day3)
import Advent.Solution.DayFour  (day4)

days :: [Day]
days = [
        day1,
        day2,
        day3,
        day4
    ]

solveDay :: Day -> Input -> IO ()
solveDay (Day d partOne partTwo) input = do
    putStrLn $ "Solving day " ++ show d
    putStrLn . (++) "Part 1: " . partOne $ input
    putStrLn . (++) "Part 2: " . partTwo $ input

solve :: Integer -> IO ()
solve day | isNothing solution = putStrLn $ "No solution for day " ++ show day
          | otherwise = do
        (year, _, _) <- toGregorian . utctDay <$> getCurrentTime
        input        <- runExceptT $ fetchInput year day
        either putStrLn (solveDay $ fromJust solution) input
    where solution = find ((==) day . number) days

