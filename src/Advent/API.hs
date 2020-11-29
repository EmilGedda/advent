{-# LANGUAGE OverloadedStrings #-}
module Advent.API where

import Prelude hiding       (readFile, null)
import Advent.Leaderboard   (Leaderboard, parseLeaderboard)
import Advent.Problem       (Input(..))
import Control.Monad.Extra  (unlessM, whenM, fromMaybeM)
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Control.Monad.Except (ExceptT(..), throwError, liftIO, liftEither, lift, mapExceptT)
import Control.Lens         ((?~), (^.))
import Data.ByteString.Lazy (ByteString, toStrict, readFile, null, stripSuffix)
import Data.Function        ((&))
import Data.Time            (UTCTime(..), parseTimeOrError, defaultTimeLocale)
import Network.HTTP.Client  (CookieJar(..), Cookie(..), createCookieJar)
import Network.Wreq         (getWith, defaults, cookies, responseBody)
import Text.Printf          (printf)
import System.Directory     (XdgDirectory(XdgConfig), getXdgDirectory, doesFileExist)
import System.FilePath      ((</>))

data NoTokenFoundError = NoTokenFoundError deriving Show

type ApiM a = ExceptT String (ReaderT CookieJar IO) a

getSessionToken :: ExceptT String IO ByteString
getSessionToken = do
    let file = (</> "session-token.txt") <$> getXdgDirectory XdgConfig "AdventOfCode"
        token = liftIO $ readFile =<< file
    unlessM (liftIO $ doesFileExist =<< file) (throwError "No session token file found")
    whenM (null <$> token) (throwError "Empty session token file")
    fromMaybeM token (stripSuffix "\n" <$> token)


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
    let opts = defaults & cookies ?~ cookiejar
    -- TODO: Error handling with Control.Exception
    liftIO $ (^. responseBody) <$> getWith opts ("https://adventofcode.com" ++ url)


input :: Integer -> Integer -> ApiM Input
input year day = Input <$> (lift . fetch $ printf "/%d/day/%d/input" year day)


leaderboard :: Integer -> Integer -> ApiM Leaderboard
leaderboard year id = liftEither . parseLeaderboard =<< lift (fetch url)
    where url = printf "/%d/leaderboard/private/view/%d.json" year id


get :: ApiM a -> ExceptT String IO a
get api = flip mapExceptT api . flip runReaderT . sessionCookie =<< getSessionToken

