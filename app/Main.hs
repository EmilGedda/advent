module Main where

import Advent.API           (get, leaderboard)
import Advent.Leaderboard   (printLeaderboard, localScore)
import Control.Monad.Except (runExceptT)


main :: IO ()
main = currentLeaderboard

currentLeaderboard :: IO ()
currentLeaderboard = either putStrLn (printLeaderboard localScore)
                     =<< (runExceptT . get $ leaderboard 2020 409260)
