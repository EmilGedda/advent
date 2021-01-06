{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ConstraintKinds #-}

module Advent.API where

import Advent.Leaderboard           (Leaderboard, parseLeaderboard, User, members, userid)
import Control.Exception            (SomeException)
import Control.Lens                 ((?~), (^.), view)
import Control.Monad                (when, unless)
import Control.Monad.Except         (ExceptT(..), throwError, lift, liftEither, lift, mapExceptT, MonadError, MonadTrans)
import Control.Monad.Reader         (Reader, ReaderT, runReader,  ask, runReaderT, MonadIO, MonadReader, liftIO)
import qualified Data.ByteString    (readFile)
import Data.Bool                    (bool)
import Data.ByteString              (ByteString, null, stripSuffix)
import Data.ByteString.Char8        (readInteger)
import Data.ByteString.Lazy         (toStrict, fromStrict)
import Data.List                    (find)
import Data.Maybe                   (fromMaybe, listToMaybe, fromJust, isNothing)
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

import Control.Monad.Catch          (MonadCatch)
import qualified Control.Monad.Catch as C

type Trans c m a = forall m1 t. (MonadTrans t, c m1, m ~ t m1) => m a

class Monad m => MonadFS m where
    tokenFile :: m FilePath
    readFile  :: FilePath  -> m ByteString
    hasFile   :: FilePath -> m Bool

    default tokenFile :: Trans MonadFS m FilePath
    tokenFile = lift tokenFile

    default readFile :: FilePath -> Trans MonadFS m ByteString
    readFile = lift . readFile

    default hasFile :: FilePath -> Trans MonadFS m Bool
    hasFile = lift . hasFile

class Monad m => MonadTime m where
    currentYear :: m Integer

    default currentYear :: Trans MonadTime m Integer
    currentYear = lift currentYear

class Monad m => MonadHTTP m where
    httpGet :: String -> CookieJar -> m ByteString

    default httpGet :: String -> CookieJar -> Trans MonadHTTP m ByteString
    httpGet x = lift . httpGet x


instance MonadFS IO where
    tokenFile = (</> "session-token.txt") <$> getXdgDirectory XdgConfig "AdventOfCode"
    readFile  = Data.ByteString.readFile
    hasFile   = doesFileExist

instance MonadTime IO where
    currentYear = do
        (year, month, _) <- toGregorian . utctDay <$> liftIO getCurrentTime
        return $ bool year (year - 1) (month < 12)

instance MonadHTTP IO where
    httpGet url cookiejar = liftIO
                         $  toStrict . view responseBody
                        <$> getWith (cookies ?~ cookiejar $ defaults) url


instance MonadFS   m => MonadFS   (ExceptT e m)
instance MonadTime m => MonadTime (ExceptT e m)
instance MonadHTTP m => MonadHTTP (ExceptT e m)
instance MonadHTTP m => MonadHTTP (ReaderT r m)


baseURL :: String
baseURL = "https://adventofcode.com"

catch :: (MonadCatch m, MonadError e m) => m a -> e -> m a
catch e str = e `C.catch` (throwError . anyException str)

anyException :: a -> SomeException -> a
anyException = const

getSessionToken :: (MonadFS m, MonadError String m, MonadCatch m) => m ByteString
getSessionToken = flip catch "Unable to read session token" $ do
    file   <- tokenFile
    exists <- hasFile file
    unless exists . throwError $ "No session token file found: " ++ file
    token  <- readFile file
    when (null token) $ throwError "Empty session token file"
    return . fromMaybe token $ stripSuffix "\n" token


sessionCookie :: ByteString -> CookieJar
sessionCookie token =
    let parseTime = parseTimeOrError True defaultTimeLocale "%Y"
        expire = parseTime "2030" :: UTCTime
        create = parseTime "2020" :: UTCTime
    in createCookieJar [Cookie "session" token expire
        "adventofcode.com" "/" create create True True False False]


fetch :: (MonadError String m, MonadReader CookieJar m, MonadHTTP m) => String -> m ByteString
fetch url = httpGet (baseURL ++ url) =<< ask


input :: (MonadError String m, MonadReader CookieJar m, MonadHTTP m, MonadCatch m)
    => Integer -> Integer -> m ByteString
input year day = fetch url `catch` "Unable to fetch input"
    where url = printf "/%d/day/%d/input" year day


currentUser :: (MonadError String m, MonadFS m, MonadHTTP m, MonadTime m, MonadCatch m) => m User
currentUser = do
    id <- get (findID =<< fetch "/settings") `catch` "Unable to fetch UserID"
    year <- currentYear
    leaderboard <- get (leaderboard year id)
    let user = find ((id ==) . userid) $ members leaderboard
    when (isNothing user) $ throwError "Unable to find user"
    return $ fromJust user


findID :: MonadError String m => ByteString -> m Integer
findID str =
    let (_, _, _, id) = str =~ ("anonymous user #([0-9]+)" :: ByteString)
                          :: (ByteString, ByteString, ByteString, [ByteString])
    in maybe (throwError "Unable to find user id") (return . fst) (readInteger =<< listToMaybe id)


leaderboard :: (MonadError String m, MonadReader CookieJar m, MonadHTTP m, MonadCatch m)
            => Integer -> Integer -> m Leaderboard
leaderboard year id = liftEither . parseLeaderboard
                  =<< fmap fromStrict (fetch url) `catch` "Unable to fetch leaderboard"
    where url = printf "/%d/leaderboard/private/view/%d.json" year id


get :: (MonadError String m, MonadFS m, MonadCatch m) => ReaderT CookieJar m a -> m a
get api = runReaderT api . sessionCookie =<< getSessionToken
