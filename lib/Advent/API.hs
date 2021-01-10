{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ConstraintKinds #-}

module Advent.API where

import           Advent.Leaderboard             (Leaderboard, parseLeaderboard, User, members, userid)
import           Control.Exception              (SomeException, displayException)
import           Control.Lens                   ((?~), (^.), view)
import           Control.Monad                  (when, unless, (<=<))
import           Control.Monad.Catch            (MonadCatch)
import           Control.Monad.Except           (ExceptT(..), throwError, lift, liftEither, lift, mapExceptT, MonadError, MonadTrans)
import           Control.Monad.Reader           (Reader, ReaderT(..), runReader,  ask, runReaderT, MonadIO, MonadReader, liftIO)
import qualified Data.ByteString                (readFile, writeFile, pack)
import           Data.Bool                      (bool)
import           Data.ByteString                (ByteString, null, stripSuffix)
import           Data.ByteString.Char8          (readInteger, pack)
import           Data.ByteString.Lazy           (toStrict, fromStrict)
import           Data.List                      (find)
import           Data.Maybe                     (fromMaybe, listToMaybe, fromJust, isNothing)
import           Data.Time                      (UTCTime, parseTimeOrError, defaultTimeLocale)
import           Data.Time.Calendar             (toGregorian)
import           Data.Time.Clock                (getCurrentTime, utctDay)
import           Network.HTTP.Client            (CookieJar, Cookie(..), createCookieJar)
import           Network.HTTP.Client.OpenSSL    (opensslManagerSettings, defaultMakeContext, defaultOpenSSLSettings)
import           Network.Wreq                   (getWith, defaults, cookies, responseBody)
import           Prelude hiding                 (readFile, writeFile, null)
import           System.Directory               (XdgDirectory(XdgConfig), getXdgDirectory, doesFileExist, createDirectoryIfMissing, getAccessTime, removeFile)
import           System.FilePath                ((</>))
import           Text.Printf                    (printf)
import           Text.Regex.TDFA                ((=~))
import           Text.Regex.TDFA.ByteString     ()
import qualified Control.Monad.Catch        as  C
import qualified Network.Wreq.Session       as  S

type Trans c m a = forall m1 t. (MonadTrans t, c m1, m ~ t m1) => m a

class Monad m => MonadFS m where
    cacheDir  :: m FilePath
    createDir :: FilePath -> m ()

    readFile  :: FilePath  -> m ByteString
    writeFile :: FilePath -> ByteString -> m ()
    hasFile   :: FilePath -> m Bool
    accessTime :: FilePath -> m UTCTime
    deleteFile :: FilePath -> m ()

    tokenFile :: m FilePath
    tokenFile = (</> "session-token.txt") <$> cacheDir

    default cacheDir :: Trans MonadFS m FilePath
    cacheDir = lift cacheDir

    default createDir :: FilePath -> Trans MonadFS m ()
    createDir = lift . createDir

    default readFile :: FilePath -> Trans MonadFS m ByteString
    readFile = lift . readFile

    default writeFile :: FilePath -> ByteString -> Trans MonadFS m ()
    writeFile path = lift . writeFile path

    default hasFile :: FilePath -> Trans MonadFS m Bool
    hasFile = lift . hasFile

    default accessTime :: FilePath -> Trans MonadFS m UTCTime
    accessTime = lift . accessTime

    default deleteFile :: FilePath -> Trans MonadFS m ()
    deleteFile = lift . deleteFile

class Monad m => MonadTime m where
    currentYear :: m Integer

    default currentYear :: Trans MonadTime m Integer
    currentYear = lift currentYear

class Monad m => MonadHTTP m where
    httpGet :: String -> m ByteString

    baseURL :: m String
    baseURL = return "https://adventofcode.com"

    default httpGet :: String -> Trans MonadHTTP m ByteString
    httpGet = lift . httpGet

instance MonadHTTP IO where
    httpGet = fmap (toStrict . view responseBody) . getWith defaults

instance MonadFS IO where
    createDir  = createDirectoryIfMissing True
    cacheDir   = getXdgDirectory XdgConfig "AdventOfCode"
    readFile   = Data.ByteString.readFile
    writeFile  = Data.ByteString.writeFile
    hasFile    = doesFileExist
    accessTime = getAccessTime
    deleteFile = System.Directory.removeFile

instance MonadTime IO where
    currentYear = do
        (year, month, _) <- toGregorian . utctDay <$> liftIO getCurrentTime
        return $ bool year (year - 1) (month < 12)


instance MonadFS   m => MonadFS   (ExceptT e m)
instance MonadFS   m => MonadFS   (ReaderT e m)
instance MonadTime m => MonadTime (ExceptT e m)
instance MonadTime m => MonadTime (ReaderT r m)
instance MonadHTTP m => MonadHTTP (ExceptT e m)


instance MonadIO m => MonadHTTP (ReaderT CookieJar m) where
    httpGet = ReaderT . flip (\cookiejar -> liftIO . fmap (toStrict . view responseBody)
            . getWith (cookies ?~ cookiejar $ defaults))

instance MonadIO m => MonadHTTP (ReaderT S.Session m) where
    httpGet = ReaderT . flip (\sess -> liftIO . fmap (toStrict . view responseBody) . S.get sess)

catch :: (MonadCatch m, MonadError String m) => m a -> String -> m a
catch e str = e `C.catch` (throwError . anyException str)

anyException :: String -> SomeException -> String
anyException a e = a ++ ": " ++ displayException e

getSessionToken :: (MonadFS m, MonadError String m, MonadCatch m) => m ByteString
getSessionToken = flip catch "Unable to read session token" $ do
    file   <- tokenFile
    exists <- hasFile file
    unless exists . throwError $ "No session token file found: " ++ file
    token  <- readFile file
    when (null token) $ throwError "Empty session token file"
    return . fromMaybe token $ stripSuffix "\n" token

session :: (MonadIO m, MonadError String m) => ByteString -> m S.Session
session = liftIO . flip S.newSessionControl (opensslManagerSettings . defaultMakeContext $ defaultOpenSSLSettings) . Just . sessionCookie

runSession :: (MonadIO m, MonadError String m, MonadFS m, MonadCatch m)
    => ReaderT S.Session m b -> m b
runSession r = runReaderT r =<< session =<< getSessionToken

runCookies :: (MonadIO m, MonadError String m, MonadFS m, MonadCatch m)
    => ReaderT CookieJar m b -> m b
runCookies r = runReaderT r . sessionCookie =<< getSessionToken

sessionCookie :: ByteString -> CookieJar
sessionCookie token =
    let parseTime = parseTimeOrError True defaultTimeLocale "%Y"
        expire = parseTime "2030" :: UTCTime
        create = parseTime "2020" :: UTCTime
    in createCookieJar [Cookie "session" token expire
        "adventofcode.com" "/" create create True True False False]

fetch :: (MonadError String m, MonadHTTP m) => String -> m ByteString
fetch url = httpGet . (++ url) =<< baseURL


input :: (MonadError String m, MonadHTTP m, MonadCatch m)
    => Integer -> Integer -> m ByteString
input year day = fetch url `catch` "Unable to fetch input"
    where url = printf "/%d/day/%d/input" year day

currentUserID :: (MonadError String m, MonadFS m, MonadHTTP m, MonadTime m, MonadCatch m) => m Integer
currentUserID = do
    cache <- (</> "user.txt") <$> cacheDir
    exist <- hasFile cache
    if not exist
       then do
           id <- fetchUserID
           writeFile cache . pack $ show id
           return id
       else do
           latest <- accessTime cache
           token <- accessTime =<< tokenFile
           if token > latest
              then deleteFile cache *> currentUserID
              else maybe (deleteFile cache *> currentUserID) (return . fst)
                 . readInteger
                =<< readFile cache


fetchUserID :: (MonadError String m, MonadFS m, MonadHTTP m, MonadTime m, MonadCatch m) => m Integer
fetchUserID = (findID =<< fetch "/settings") `catch` "Unable to fetch UserID"

currentUser :: (MonadError String m, MonadFS m, MonadHTTP m, MonadTime m, MonadCatch m) => m User
currentUser = do
    id <- currentUserID
    year <- currentYear
    maybe (throwError "Unable to find user") return
        . find ((id ==) . userid)
        . members =<< leaderboard year id


findID :: MonadError String m => ByteString -> m Integer
findID str =
    let (_, _, _, id) = str =~ ("anonymous user #([0-9]+)" :: ByteString)
                          :: (ByteString, ByteString, ByteString, [ByteString])
    in maybe (throwError "Unable to find user id")
             (return . fst)
             (readInteger =<< listToMaybe id)


leaderboard :: (MonadError String m, MonadHTTP m, MonadCatch m)
            => Integer -> Integer -> m Leaderboard
leaderboard year id = ((liftEither . parseLeaderboard) . fromStrict =<< fetch url)
                      `catch` "Unable to fetch leaderboard"
    where url = printf "/%d/leaderboard/private/view/%d.json" year id
