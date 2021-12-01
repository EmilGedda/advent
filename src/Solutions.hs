{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PartialTypeSignatures #-}
module Solutions where

import Advent
import Advent.API
import Advent.Problem
import Solutions.Y2020
import Solutions.Y2021

import Control.Monad.Except      (runExceptT)
import Data.List                 (find)


years :: Years
years = [ WrapYear y2020, WrapYear y2021]

solveDay :: Day n -> Input -> IO ()
solveDay d@Day{ partOne, partTwo } (Input text) = do
    putStrLn $ "Solving day " ++ show (dayNum d)
    putStrLn $ "Part 1: " ++ solution (partOne $ parseInput text)
    putStrLn $ "Part 2: " ++ solution (partTwo $ parseInput text)

solve :: Integer -> IO ()
solve day = solve' day =<< currentYear

solve' ::  Integer -> Integer -> IO ()
solve' day y =
        maybe (putStrLn $ "No solution for year " ++ show y)
              (\(WrapYear (Year days)) -> do
                input <- runExceptT . runNetworkEnv $ fetchInput y day
                maybe (putStrLn $ "No solution for day " ++ show day)
                      (\(WrapDay d) -> either putStrLn (solveDay d) input)
                      $ find ((==) day . someDayNum) $ toDayList days)
              $ find (\(WrapYear y') -> yearNum y' == y) years


