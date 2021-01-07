{-# LANGUAGE BangPatterns #-}
module Solutions where

import Advent.Problem            (Day(..), Year(..), days, Input, fetchInput, fromInput, parseInput, solution)
import Advent.API                (currentYear)
import Control.Monad.Except      (runExceptT, liftIO)
import Data.Maybe                (isNothing, fromJust, maybe)
import Data.List                 (find)

import Solutions.Y2020           (y2020)

years :: [Year]
years = [
        y2020
    ]

solveDay :: Day -> Input -> IO ()
solveDay Day{ number=d, partOne=first, partTwo=second } !text = do
    putStrLn $ "Parsing day " ++ show d
    let input = parseInput $ fromInput text
    putStrLn $ "Parsing 1: " ++ solution (first input)
    putStrLn $ "Parsing 2: " ++ solution (second input)


solve :: Integer -> IO ()
solve day = do
        now  <- currentYear
        maybe (putStrLn $ "No solution for year" ++ show now)
              (\(Year _ days) -> do
                input <- runExceptT $ fetchInput now day
                maybe (liftIO . putStrLn $ "No solution for day " ++ show day)
                      (\ans -> either putStrLn (solveDay ans) input) (answer days)
              ) (from now)
        where answer = find ((==) day . number)
              from now = find ((==) now . year) years

