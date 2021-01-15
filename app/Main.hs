{-# LANGUAGE TemplateHaskell #-}
module Main where

import           Advent
import           Advent.API
import           Advent.SVG
import           Advent.Leaderboard
import           Control.Monad.Except   (runExceptT, ExceptT(..))
import           Control.Monad.Reader   (ReaderT)
import           Data.Bool              (bool)
import           Data.Version           (showVersion)
import           Development.GitRev     (gitHash)
import           Data.Char              (toLower)
import           Data.List              (intercalate)
import           Data.Maybe             (fromMaybe)
import           Data.Time              (getCurrentTime)
import           Data.Time.Format       (formatTime, defaultTimeLocale)
import           Network.Wreq.Session   (Session)
import           Network.HTTP.Client    (CookieJar)
import           Paths_advent           (version)
import           Options.Applicative
import           Language.Haskell.TH
import qualified Data.ByteString.Char8  as B

type App r a = ReaderT r (ExceptT String IO) a


data LeaderboardOrder = LocalScore | Stars

data Options = LeaderboardOptions {
                lid :: Maybe Integer,
                year :: Maybe Integer,
                order :: User -> Integer
            } | ProgressOptions {
                onlyStarCount :: Bool
            } | BadgesOptions {
                color :: Color
            } | VersionOptions

colorReader :: ReadM Color
colorReader = eitherReader $ \s ->
        maybe (Left $ help s) Right $ map toLower s `lookup` order
        where order = [("gold", Gold), ("silver", Silver)]
              help s = "Could not parser color \"" ++ s
                       ++ "\", expected one of: " ++ intercalate ", " (map fst order)


orderReader :: ReadM (User -> Integer)
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


badgesParser :: Parser Options
badgesParser = BadgesOptions
        <$> argument colorReader
                (help "Color of star to generate. Gold or silver."
                <> metavar "COLOR")

versionParser :: Parser Options
versionParser = flag' VersionOptions
                    (long "version"
                    <> help "Display advent version")


optionsParser :: Parser Options
optionsParser = subparser $
    command "badge"       (badgesParser `withInfo` "Generate a badge from current user progress") <>
    command "leaderboard" (leaderboardParser `withInfo` "Display a leaderboard") <>
    command "progress"    (progressParser `withInfo` "Show current user progress")


withInfo :: Parser a -> String -> ParserInfo a
withInfo opts desc = info (helper <*> opts) $ progDesc desc

main :: IO ()
main = run =<< customExecParser (prefs showHelpOnError)
        (info (helper <*> versionParser <|> optionsParser)
              (fullDesc
              <> progDesc "Display info and stats from Advent of Code"
              <> header "advent - Advent of Code in your terminal"))

output :: (a -> IO (Either String b)) -> (b -> IO ()) -> a -> IO ()
output f g input = either putStrLn g =<< f input

(<==) :: (a -> IO ()) -> App Session a -> IO ()
(<==) = output $ runExceptT . runSession
infixr 0 <==

(<<=) :: (a -> IO ()) -> App CookieJar a -> IO ()
(<<=) = output $ runExceptT .  runCookies
infixr 0 <<=

run :: Options -> IO ()
run (LeaderboardOptions id year order)
  = putStr <== do
        now <- currentYear
        id' <- maybe currentUserID return id
        l <- leaderboard (fromMaybe now year) id'
        prettyLeaderboard order l

run (ProgressOptions onlyStarCount)
    | onlyStarCount = printStars . starCount <== currentUser
    | otherwise = putStrLn <== do
        now <- currentYear
        id <- currentUserID
        soloboard <- leaderboard now id
        prettyLeaderboard stars
            $ soloboard { members = filter ((==) id . userid) (members soloboard) }
    where printStars (silver, gold) = do
            putStr "Silver\t"
            print silver
            putStr "Gold\t"
            print gold

run (BadgesOptions color)
  = B.putStrLn
  . badge color
  . bool snd fst (color == Silver)
  . starCount
  <== currentUser

run VersionOptions
  = putStrLn
  $ "advent ("
    <> take 8 $(gitHash)
    <> ") - v"
    <> showVersion version
    <> " // "
    <> $(stringE =<< runIO (formatTime defaultTimeLocale "%F %T %EZ" <$> getCurrentTime))
