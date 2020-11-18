module Main where

import Advent.API.Leaderboard   (printLeaderboard, parseLeaderboard, testJson)
import Advent.Solutions         (days)
import Advent.Problem           (Input(..), Day(..), toString)
import Control.Monad            (guard)
import Data.Maybe               (maybe, isNothing, fromJust)
import Data.Either              (rights)
import System.CPUTime           (getCPUTime)
import Text.Printf

main :: IO ()
main = printLeaderboard . head . rights $ [parseLeaderboard testJson]

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
            answer <- return $ solvePart ([partOne, fromJust partTwo] !! (part - 1))
            end <- getCPUTime

            let duration = (fromIntegral (end - start)) / (10^9)
            printf "%s (%0.3fms)\n" answer (duration :: Double)

    in mapM_ printPart [1,2]

fetchInput :: Integer -> Input
fetchInput _ = Input "fetchInput not implemented"
