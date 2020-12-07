module Main where

import Advent.API
import Advent.Problem
import Advent.Leaderboard

import Control.Monad.Except (runExceptT, ExceptT(..), mapExceptT)
import Data.Char            (toLower)
import Data.List            (partition, intercalate)
import Data.Maybe           (fromMaybe)
import Options.Applicative
import qualified Data.Map as M

data LeaderboardOrder = LocalScore | Stars

data BadgesOptions = Path String | Disabled

data Options = LeaderboardOptions {
                id :: Maybe Integer,
                year :: Maybe Integer,
                order :: User -> Integer
            } | ProgressOptions {
                onlyStarCount :: Bool
            } | BadgesOptions {
                gold :: BadgesOptions,
                silver :: BadgesOptions
            }

orderReader = eitherReader $ \s ->
        maybe (Left $ help s) Right $ map toLower s `lookup` order
        where order = [("localscore", localScore), ("stars", stars)]
              help s = "Could not parser order \"" ++ s
                       ++ "\", expected one of: " ++ intercalate ", " (map fst order)


leaderboardParser :: Parser Options
leaderboardParser = LeaderboardOptions
        <$> optional
                (option auto
                    (long "id"
                    <> short 'i'
                    <> metavar "ID"
                    <> help ("Leaderboard ID. Defaults to the private leaderboard "
                            ++ "of the current user. Global leaderboard not supported.")))
        <*> optional
                (option auto
                    (long "year"
                    <> short 'y'
                    <> metavar "YEAR"
                    <> help "Year of Leaderboard. Defaults to current year."))
        <*> option orderReader
                    (long "order"
                    <> short 'o'
                    <> metavar "ORDER"
                    <> value stars
                    <> help ("Scoring order, can be \"localscore\" or \"stars\". "
                          ++ "Defaults to stars. Ties are resolved by recency of last star."))

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

    let id'   = mapExceptT (fmap $ getID id) currentUser
        year' = fromMaybe now   year

    printLeaderboard order <== get . leaderboard year' =<< id'

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
