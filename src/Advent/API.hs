{-# LANGUAGE OverloadedStrings #-}
module Advent.API where

import Advent.Leaderboard           (Leaderboard, parseLeaderboard, User, members, userid)
import Control.Exception            (SomeException)
import Control.Lens                 ((?~), (^.))
import Control.Monad                (when, unless)
import Control.Monad.Except         (ExceptT, throwError, lift, liftEither, lift, mapExceptT, MonadError)
import Control.Monad.Reader         (ReaderT, ask, runReaderT)
import Data.ByteString.Lazy         (ByteString, toStrict, readFile, null, stripSuffix)
import Data.ByteString.Lazy.Char8   (readInteger)
import Data.List                    (find)
import Data.Maybe                   (fromMaybe, listToMaybe)
import Data.Time                    (UTCTime, parseTimeOrError, defaultTimeLocale)
import Data.Time.Calendar           (toGregorian)
import Data.Time.Clock              (getCurrentTime, utctDay)
import Network.HTTP.Client          (CookieJar, Cookie(..), createCookieJar)
import Network.Wreq                 (getWith, defaults, cookies, responseBody)
import Prelude hiding               (readFile, null)
import System.Directory             (XdgDirectory(XdgConfig), getXdgDirectory, doesFileExist)
import System.FilePath              ((</>))
import Text.Printf                  (printf)
import Text.Regex.TDFA              ((=~))
import Text.Regex.TDFA.ByteString   ()

import qualified Control.Monad.Catch as C

data NoTokenFoundError = NoTokenFoundError deriving Show

type ApiM a = ExceptT String (ReaderT CookieJar IO) a

baseURL :: String
baseURL = "https://adventofcode.com"

catch :: (C.MonadCatch m, MonadError e m) => m a -> e -> m a
catch e str = e `C.catch` (throwError . anyException str)

anyException :: a -> SomeException -> a
anyException = const

currentYear :: IO Integer
currentYear = do
    (year, _, _) <- toGregorian . utctDay <$> getCurrentTime
    return year


getSessionToken :: ExceptT String IO ByteString
getSessionToken = flip catch "Unable to read session token" $ do
    file    <- lift $ (</> "session-token.txt") <$> getXdgDirectory XdgConfig "AdventOfCode"
    hasFile <- lift $ doesFileExist file
    unless  hasFile . throwError $ "No session token file found: " ++ file
    token   <- lift $ readFile file
    when (null token) $ throwError "Empty session token file"
    return . fromMaybe token $ stripSuffix "\n" token


sessionCookie :: ByteString -> CookieJar
sessionCookie token =
    let parseTime = parseTimeOrError True defaultTimeLocale "%Y"
        expire = parseTime "2030" :: UTCTime
        create = parseTime "2020" :: UTCTime
    in createCookieJar [Cookie "session" (toStrict token) expire
        "adventofcode.com" "/" create create True True False False]


fetch :: String -> ApiM ByteString
fetch url = lift $ do
    cookiejar <- ask
    let opts = cookies ?~ cookiejar $ defaults
    response <- lift $ getWith opts (baseURL ++ url)
    return $ response ^. responseBody


input :: Integer -> Integer -> ApiM ByteString
input year day = fetch url `catch` "Unable to fetch input"
    where url = printf "/%d/day/%d/input" year day


currentUser :: ExceptT String IO User
currentUser = do
    id <- get $ liftEither . findID =<< fetch "/settings" `catch` "Unable to fetch UserID"
    year <- lift currentYear
    leaderboard <- get (leaderboard year id)
    let users = find ((id ==) . userid) $ members leaderboard
    liftEither $ maybe (Left "Unable to find user") Right users


findID :: ByteString -> Either String Integer
findID str =
    let (_, _, _, id) = str =~ ("anonymous user #([0-9]+)" :: ByteString)
                          :: (ByteString, ByteString, ByteString, [ByteString])
    in maybe (Left "Unable to find user id") (Right . fst) (readInteger =<< listToMaybe id)


leaderboard :: Integer -> Integer -> ApiM Leaderboard
leaderboard year id = liftEither . parseLeaderboard =<< fetch url `catch` "Unable to fetch leaderboard"
    where url = printf "/%d/leaderboard/private/view/%d.json" year id


get :: ApiM a -> ExceptT String IO a
get api = flip mapExceptT api . flip runReaderT . sessionCookie =<< getSessionToken
