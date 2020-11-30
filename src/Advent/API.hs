{-# LANGUAGE OverloadedStrings #-}
module Advent.API where

import Prelude hiding       (readFile, null)
import Advent.Leaderboard   (Leaderboard, parseLeaderboard)
import Control.Monad        (when, unless)
import Control.Exception    (SomeException)
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Control.Monad.Except (ExceptT, throwError, lift, liftEither, lift, mapExceptT, MonadError)
import Control.Lens         ((?~), (^.))
import Data.ByteString.Lazy (ByteString, toStrict, readFile, null, stripSuffix)
import Data.Maybe           (fromMaybe)
import Data.Time            (UTCTime, parseTimeOrError, defaultTimeLocale)
import Network.HTTP.Client  (CookieJar, Cookie(..), createCookieJar)
import Network.Wreq         (getWith, defaults, cookies, responseBody)
import Text.Printf          (printf)
import System.Directory     (XdgDirectory(XdgConfig), getXdgDirectory, doesFileExist)
import System.FilePath      ((</>))

import qualified Control.Monad.Catch as C

data NoTokenFoundError = NoTokenFoundError deriving Show

type ApiM a = ExceptT String (ReaderT CookieJar IO) a

baseURL :: String
baseURL = "https://adventofcode.com"


catch :: (C.MonadCatch m, MonadError e m) => m a -> e -> m a
catch e str = e `C.catch` (throwError . anyException str)


anyException :: a -> SomeException -> a
anyException = const


getSessionToken :: ExceptT String IO ByteString
getSessionToken = flip catch "Unable to read session token" $ do
    file    <- lift $ (</> "session-token.txt") <$> getXdgDirectory XdgConfig "AdventOfCode"
    hasFile <- lift $ doesFileExist file
    unless  hasFile $ throwError "No session token file found"
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


fetch :: String -> ReaderT CookieJar IO ByteString
fetch url = do
    cookiejar <- ask
    let opts = cookies ?~ cookiejar $ defaults
    response <- lift $ getWith opts (baseURL ++ url)
    return $ response ^. responseBody


input :: Integer -> Integer -> ApiM ByteString
input year day = lift (fetch url) `catch` "Unable to fetch input"
    where url = printf "/%d/day/%d/input" year day


leaderboard :: Integer -> Integer -> ApiM Leaderboard
leaderboard year id = liftEither . parseLeaderboard =<< lift (fetch url) `catch` "Unable to fetch leaderboard"
    where url = printf "/%d/leaderboard/private/view/%d.json" year id


get :: ApiM a -> ExceptT String IO a
get api = flip mapExceptT api . flip runReaderT . sessionCookie =<< getSessionToken
