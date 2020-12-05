module Main where

import Advent.API
import Advent.Leaderboard

import Control.Monad.Except (runExceptT, ExceptT(..))
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
                    <> metavar "localscore|globalscore|stars"))

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


withInfo :: Parser a -> String -> ParserInfo a
withInfo opts desc = info (helper <*> opts) $ progDesc desc

toOrder :: LeaderboardOrder -> (User -> Integer)
toOrder LocalScore = localScore
toOrder Stars = stars

getID :: Either String Integer -> Maybe Integer -> Either String Integer
getID err@(Left _) = const err
getID (Right id) = Right . fromMaybe id

run :: Options -> IO ()
run (LeaderboardOptions id year order) = do
    now <- currentYear
    user <- runExceptT $ userid <$> currentUser

    let id'    = getID user id
        year'  = fromMaybe now   year
        order' = maybe stars toOrder order

    board <- runExceptT $ get . leaderboard year' =<< ExceptT (return id')
    either putStrLn (printLeaderboard order') board

run (ProgressOptions onlyStarCount) = do
    now <- currentYear

    let user = currentUser
        id   = userid <$> user
        output = either putStrLn
        soloboard id = do
                l <- get $ leaderboard now id
                return $ l { members = filter ((==) id . userid) (members l) }

        printStars silver gold = do
            putStr "Silver\t"
            print silver
            putStr "Gold\t"
            print gold

    if onlyStarCount
       then output (uncurry printStars . starCount . progress) =<< runExceptT user
       else output (printLeaderboard stars) =<< runExceptT (soloboard =<< id)


starCount :: M.Map Integer Progress -> (Int, Int)
starCount = (length *** length) . partition (==1) . map fromProgress . M.elems
