{-# LANGUAGE NamedFieldPuns #-}
module Solutions where

import Advent
import Advent.API
import Advent.Problem
import Solutions.Y2020

import Control.Monad.Except      (runExceptT)
import Data.List                 (find)


years :: [Year]
years = [
        y2020
    ]

solveDay :: Day -> Input -> IO ()
solveDay Day{ number, partOne, partTwo } (Input text) = do
    putStrLn $ "Parsing day " ++ show number
    putStrLn $ "Parsing 1: " ++ solution (partOne $ parseInput text)
    putStrLn $ "Parsing 2: " ++ solution (partTwo $ parseInput text)

solve :: Integer -> IO ()
solve day = solve' day =<< currentYear

solve' ::  Integer -> Integer -> IO ()
solve' day y =
        maybe (putStrLn $ "No solution for year" ++ show y)
              (\(Year _ days) -> do
                input <- runExceptT . runSession $ fetchInput y day
                maybe (putStrLn $ "No solution for day " ++ show day)
                      (\ans -> either putStrLn (solveDay ans) input)
                      $ find ((==) day . number) days)
              $ find ((==) y . year) years


