module Main where

import Advent.API
import Advent.Problem
import Advent.Leaderboard

import           Control.Monad.Except   (runExceptT, ExceptT(..), mapExceptT)
import           Control.Monad.Catch    (MonadCatch)
import           Control.Monad.Reader   (ReaderT)
import           Data.Char              (toLower)
import           Data.List              (partition, intercalate)
import           Data.Maybe             (fromMaybe)
import           Network.Wreq.Session   (Session)
import           Network.HTTP.Client    (CookieJar)
import           Options.Applicative
import qualified Data.Map as M

type App a = ReaderT Session (ExceptT String IO) a

data LeaderboardOrder = LocalScore | Stars

data BadgesOptions = Path String | Disabled

data Options = LeaderboardOptions {
                lid :: Maybe Integer,
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


output :: (a -> IO ()) -> App a -> IO ()
output f input = either putStrLn f
  =<< (runExceptT . runSession) input

(<==) = output
infixr 0 <==

withInfo :: Parser a -> String -> ParserInfo a
withInfo opts desc = info (helper <*> opts) $ progDesc desc

toOrder :: LeaderboardOrder -> (User -> Integer)
toOrder LocalScore = localScore
toOrder Stars = stars

getID :: Maybe Integer -> User -> Integer
getID override = flip fromMaybe override . userid

current :: App (Integer, User)
current = (,) <$> currentYear <*> currentUser

run :: Options -> IO ()
run (LeaderboardOptions id year order)
  = printLeaderboard order <== do
        (now, user) <- current
        leaderboard (fromMaybe now year) (getID id user)

run (ProgressOptions onlyStarCount)
    | onlyStarCount = printStars . starCount . progress <== currentUser
    | otherwise = printLeaderboard stars <== do
        (now, user) <- current
        let id = userid user
        soloboard <- leaderboard now id
        return $ soloboard { members = filter ((==) id . userid) (members soloboard) }
    where printStars (silver, gold) = do
            putStr "Silver\t"
            print silver
            putStr "Gold\t"
            print gold


starCount :: M.Map Integer Progress -> (Int, Int)
starCount = both length . partition (==1) . map fromProgress . M.elems

