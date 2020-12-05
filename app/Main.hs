module Main where

import Advent.API           (get, leaderboard, currentUser)
import Advent.Leaderboard   (printLeaderboard, stars, progress, Progress, fromProgress)
import Control.Monad.Except (runExceptT)
import Control.Arrow        ((***))
import Data.List            (partition)
import qualified Data.Map as M


main :: IO ()
main = shortProgress


starCount :: M.Map Integer Progress -> (Int, Int)
starCount = (length *** length) . partition (==1) . map fromProgress . M.elems


shortProgress :: IO ()
shortProgress = run (uncurry printStars . starCount . progress) =<< runExceptT currentUser
    where printStars silver gold = do
            putStr "Silver:\t"
            print silver
            putStr "Gold:\t"
            print gold


currentLeaderboard :: IO ()
currentLeaderboard = run (printLeaderboard stars)
                     =<< (runExceptT . get $ leaderboard 2020 409260)

run = either putStrLn
