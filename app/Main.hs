{-# LANGUAGE OverloadedStrings #-}
module Main where

import Advent.Leaderboard   (printLeaderboard, stars)
import Advent.API           (get, leaderboard)
import Control.Monad.Except (runExceptT)

main :: IO ()
main = currentLeaderboard


currentLeaderboard = either putStrLn (printLeaderboard stars)
                     =<< (runExceptT . get $ leaderboard 2020 409260)
