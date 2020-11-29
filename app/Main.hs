{-# LANGUAGE OverloadedStrings #-}
module Main where

import Advent.Leaderboard   (printLeaderboard, stars)
import Advent.API           (get, leaderboard)
import Advent.Problem       (Input(..), Day(..), toString)
import Control.Monad.Except (runExceptT)
import Data.Maybe           (isNothing, fromJust)
import System.CPUTime       (getCPUTime)
import Text.Printf          (printf)

main :: IO ()
main = currentLeaderboard


currentLeaderboard = either print (printLeaderboard stars)
                     =<< (runExceptT . get $ leaderboard 2020 409260)

solve :: Day -> IO ()
solve (Day day partOne partTwo) =
    let
        input = fetchInput day
        header n = "Solving day " ++ show day ++ ", part " ++ show n ++ ":"
        solvePart = toString . ($ input)
        printPart part | isNothing partTwo && part == 2 = return ()
                       | otherwise = do
            putStrLn $ header part

            start <- getCPUTime
            let answer = solvePart ([partOne, fromJust partTwo] !! (part - 1))
            end <- getCPUTime

            let duration = fromIntegral (end - start) / (10^9)
            printf "%s (%0.3fms)\n" answer (duration :: Double)

    in mapM_ printPart [1,2]

fetchInput :: Integer -> Input
fetchInput _ = Input "fetchInput not implemented"
