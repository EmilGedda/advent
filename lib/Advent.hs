{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ConstraintKinds #-}

module Advent where

import           Prelude hiding                 (readFile, writeFile, null)
import           Control.Monad                  (liftM2)
import           Control.Lens                   ((?~), view)
import           Network.Wreq                   (getWith, defaults, cookies, responseBody)
import qualified Data.ByteString                (readFile, writeFile)
import           Data.Bool                      (bool)
import           Data.ByteString                (ByteString)
import           Data.ByteString.Lazy           (toStrict)
import           Control.Exception              (SomeException, displayException)
import           Control.Monad.Catch            (MonadCatch)
import           Control.Monad.Except           (ExceptT(..), throwError, lift, MonadError, MonadTrans)
import           Control.Monad.Reader           (ReaderT(..), MonadIO, liftIO)
import           Data.Time                      (UTCTime)
import           Data.Time.Calendar             (toGregorian)
import           Data.Time.Clock                (getCurrentTime, utctDay, secondsToNominalDiffTime)
import           Data.Time.Clock.POSIX          (getPOSIXTime, POSIXTime)
import           System.Directory               (XdgDirectory(XdgConfig), getXdgDirectory, doesFileExist, createDirectoryIfMissing, getAccessTime, removeFile)
import           System.FilePath                ((</>))
import           Network.HTTP.Client            (CookieJar)
import           Network.HTTP.Client.OpenSSL    (withOpenSSL)
import qualified Network.Wreq.Session       as  S
import qualified Control.Monad.Catch        as  C

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
    timeSince :: Integral a => a -> m POSIXTime

    default currentYear :: Trans MonadTime m Integer
    currentYear = lift currentYear

    default timeSince :: Integral a => a -> Trans MonadTime m POSIXTime
    timeSince = lift . timeSince

class Monad m => MonadHTTP m where
    httpGet :: String -> m ByteString

    baseURL :: m String
    baseURL = return "https://adventofcode.com"

    default httpGet :: String -> Trans MonadHTTP m ByteString
    httpGet = lift . httpGet

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

    timeSince =  liftM2 (-) getPOSIXTime . return
              . secondsToNominalDiffTime . fromIntegral


instance MonadFS   m => MonadFS   (ExceptT e m)
instance MonadFS   m => MonadFS   (ReaderT e m)
instance MonadTime m => MonadTime (ExceptT e m)
instance MonadTime m => MonadTime (ReaderT r m)
instance MonadHTTP m => MonadHTTP (ExceptT e m)


instance MonadIO m => MonadHTTP (ReaderT CookieJar m) where
    httpGet = ReaderT . flip (\cookiejar -> liftIO . withOpenSSL
            . fmap (toStrict . view responseBody)
            . getWith (cookies ?~ cookiejar $ defaults))

instance MonadIO m => MonadHTTP (ReaderT S.Session m) where
    httpGet = ReaderT . flip (\sess -> liftIO . withOpenSSL
            . fmap (toStrict . view responseBody) . S.get sess)

catch :: (MonadCatch m, MonadError String m) => m a -> String -> m a
catch e str = e `C.catch` (throwError . anyException str)

anyException :: String -> SomeException -> String
anyException a e = a ++ ": " ++ displayException e

