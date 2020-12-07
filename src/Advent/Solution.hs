{-# LANGUAGE BangPatterns #-}
module Advent.Solution where

import Advent.Problem           (Day(..), Input, fetchInput, fromInput, parseInput, solution)
import Advent.API               (currentYear)
import Control.Arrow            (first)
import Data.List                (genericLength)
import Control.Monad.Except     (runExceptT, replicateM)
import Data.Maybe               (isNothing, fromJust)
import Data.List                (find)
import System.CPUTime           (getCPUTime)
import Text.Printf              (printf)

import Advent.Solution.DayOne   (day1)
import Advent.Solution.DayTwo   (day2)
import Advent.Solution.DayThree (day3)
import Advent.Solution.DayFour  (day4)
import Advent.Solution.DayFive  (day5)
import Advent.Solution.DaySix   (day6)
import Advent.Solution.DaySeven (day7)

days :: [Day]
days = [
        day1,
        day2,
        day3,
        day4,
        day5,
        day6,
        day7
    ]

benchDay :: Int -> Day -> Input -> IO ()
benchDay _ Day{ number=d, partOne=first, partTwo=second } !text = do
    putStr $ "Parsing day " ++ show d

    let
        timeit !f = do
            before <- getCPUTime
            let val = f
            after <- getCPUTime
            let ms :: Double
                ms = fromIntegral (after - before) * 1e-6
            return (ms, val)

    (ms, input) <- timeit (parseInput $ fromInput text)
    printf "\t\t(%07.3fms)\n" ms

    putStr "Part 1: "
    (ms, one) <- timeit (first input)
    printf "%-16s(%07.3fms)\n" (solution one) ms

    putStr "Part 2: "
    (ms, two) <- timeit (second input)
    printf "%-16s(%07.3fms)\n" (solution two) ms


solve :: Integer -> IO ()
solve = flip bench 1

bench :: Integer -> Int -> IO ()
bench day n | isNothing solution = putStrLn $ "No solution for day " ++ show day
            | otherwise = do
        year  <- currentYear
        input <- runExceptT $ fetchInput year day
        either putStrLn (benchDay n $ fromJust solution) input
    where solution = find ((==) day . number) days

