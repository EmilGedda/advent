module Advent.Solution where

import Advent.Solution.DayOne   (day1)
import Advent.Problem           (Day(..), Input, fetchInput, toString)
import Control.Monad.Except     (runExceptT)
import Data.Maybe               (isNothing, fromJust)
import Data.List                (find)
import Data.Time.Clock          (getCurrentTime, utctDay)
import Data.Time.Calendar       (toGregorian)


days :: [Day]
days = [
    day1
    ]

solveDay :: Day -> Input -> IO ()
solveDay (Day d partOne partTwo) input = do
    putStrLn $ "Solving day " ++ show d
    putStrLn . (++) "Part 1: " . toString $ partOne input
    putStrLn . (++) "Part 2: " . toString $ partTwo input

solve :: Integer -> IO ()
solve day | isNothing solution = putStrLn $ "No solution for day " ++ show day
          | otherwise = do
        (year, _, _) <- toGregorian . utctDay <$> getCurrentTime
        input        <- runExceptT $ fetchInput year day
        either putStrLn (solveDay $ fromJust solution) input
    where solution = find ((==) day . number) days

