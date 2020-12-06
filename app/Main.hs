module Main where

import Advent.API
import Advent.Problem
import Advent.Leaderboard

import Control.Monad.Except (runExceptT, ExceptT(..), lift, mapExceptT)
import Control.Arrow        ((***))
import Data.List            (partition)
import Data.Maybe           (fromMaybe)
import Options.Applicative
import qualified Data.Map as M

data LeaderboardOrder = LocalScore | Stars deriving (Read, Show)

data Options = LeaderboardOptions {
                id :: Maybe Integer,
                year :: Maybe Integer,
                order :: Maybe LeaderboardOrder
            } | ProgressOptions {
                onlyStarCount :: Bool
            }

leaderboardParser :: Parser Options
leaderboardParser = LeaderboardOptions
        <$> optional
                (option auto
                    (long "id"
                    <> short 'i'
                    <> help "Leaderboard ID"))
        <*> optional
                (option auto
                    (long "year"
                    <> short 'y'
                    <> help "Year of Leaderboard"))
        <*> optional
                (option auto
                    (long "order"
                    <> short 'o'
                    <> help "Scoring order"
                    <> metavar "localscore|stars"))

progressParser :: Parser Options
progressParser = ProgressOptions
        <$> switch
                (long "short"
                <> short 's'
                <> help "Only display gold and silver star counts")

optionsParser :: Parser Options
optionsParser = subparser $
    command "leaderboard" (leaderboardParser `withInfo` "Display a leaderboard") <>
    command "progress"    (progressParser `withInfo` "Show current user progress")



main :: IO ()
main = run =<< customExecParser (prefs showHelpOnError)
    (optionsParser `withInfo` "Display info and stats from Advent of Code")


output :: (a -> IO ()) -> ExceptT String IO a -> IO ()
output f input = either putStrLn f =<< runExceptT input

(<==) = output
infixr 1 <==

withInfo :: Parser a -> String -> ParserInfo a
withInfo opts desc = info (helper <*> opts) $ progDesc desc

toOrder :: LeaderboardOrder -> (User -> Integer)
toOrder LocalScore = localScore
toOrder Stars = stars

getID :: Functor f => Maybe Integer -> f User -> f Integer
getID override user = flip fromMaybe override . userid <$> user

run :: Options -> IO ()
run (LeaderboardOptions id year order) = do
    now <- currentYear

    let id'    = mapExceptT (fmap $ getID id) currentUser
        year'  = fromMaybe now   year
        order' = maybe stars toOrder order

    printLeaderboard order' <== get . leaderboard year' =<< id'

run (ProgressOptions onlyStarCount) = do
    now <- currentYear

    let user = currentUser
        id   = userid <$> user
        soloboard id = do
            l <- get $ leaderboard now id
            return $ l { members = filter ((==) id . userid) (members l) }

        printStars (silver, gold) = do
            putStr "Silver\t"
            print silver
            putStr "Gold\t"
            print gold

    if onlyStarCount
       then printStars . starCount . progress <== user
       else printLeaderboard stars <== soloboard =<< id


starCount :: M.Map Integer Progress -> (Int, Int)
starCount = both length . partition (==1) . map fromProgress . M.elems
