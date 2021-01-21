{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Advent where

import           Prelude                        hiding (readFile, writeFile, null)

import           Control.Monad                  (liftM2)
import           Control.Lens                   ((?~), view, (^.))
import           Network.Wreq                   (getWith, defaults, cookies, responseBody, responseBody, statusCode, statusMessage, responseStatus, Response)
import           Data.Bool                      (bool)
import           Data.ByteString                (ByteString)
import           Data.ByteString.Lazy           (toStrict)
import           Control.Exception              (SomeException, displayException, IOException)
import           Control.Monad.Catch            (MonadCatch, fromException)
import           Control.Monad.Except           (ExceptT(..), throwError, lift, MonadError, MonadTrans)
import           Control.Monad.Reader           (ReaderT(..), MonadIO, liftIO)
import           Data.Time                      (UTCTime)
import           Data.Time.Calendar             (toGregorian)
import           Data.Time.Clock                (getCurrentTime, utctDay, secondsToNominalDiffTime)
import           Data.Time.Clock.POSIX          (getPOSIXTime, POSIXTime)
import           System.Directory               (XdgDirectory(XdgConfig), getXdgDirectory, doesFileExist, createDirectoryIfMissing, getAccessTime, removeFile)
import           Network.HTTP.Client.OpenSSL    (withOpenSSL)
import qualified Data.ByteString                (readFile, writeFile)
import qualified Data.ByteString.Char8          as B
import qualified Data.ByteString.Lazy           as BL
import qualified GHC.IO.Exception               as GHC
import qualified Network.HTTP.Client            as H
import qualified Network.Wreq.Session           as S
import qualified Control.Monad.Catch            as C

type Trans c m a = forall m1 t. (MonadTrans t, c m1, m ~ t m1) => m a

class Monad m => MonadFS m where
    cacheDir  :: m FilePath
    createDir :: FilePath -> m ()

    readFile  :: FilePath  -> m ByteString
    writeFile :: FilePath -> ByteString -> m ()
    hasFile   :: FilePath -> m Bool
    accessTime :: FilePath -> m UTCTime
    deleteFile :: FilePath -> m ()

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

wreq :: MonadIO m => (r -> a -> IO (Response BL.ByteString)) -> a -> ReaderT r m ByteString
wreq f = ReaderT . flip (\x -> liftIO . withOpenSSL . fmap (toStrict . view responseBody) . f x)

instance MonadIO m => MonadHTTP (ReaderT H.CookieJar m) where
    httpGet = wreq (getWith . flip (cookies ?~) defaults)

instance MonadIO m => MonadHTTP (ReaderT S.Session m) where
    httpGet = wreq S.get

catch :: (MonadCatch m, MonadError String m) => m a -> String -> m a
catch e str = e `C.catch` (throwError . prettyException str)

httpCatcher :: H.HttpException -> String
httpCatcher (H.HttpExceptionRequest req content)
    = case content of -- HTTP Exceptions
        H.StatusCodeException res _
             -> "expected 200 OK but got "
                ++ show (res ^. responseStatus . statusCode)
                ++ " "
                ++ B.unpack (res ^. responseStatus . statusMessage)
                ++ " during "
                ++ B.unpack (H.method req)
                ++ " "
                ++ show (H.getUri req)

        H.ConnectionFailure conerr
            -> flip prettyException conerr
                $ "connecting to "
                ++ show (H.getUri req)
                ++ " failed"

        _ -> show content

httpCatcher err = displayException err

ioCatcher :: IOException -> String
ioCatcher (GHC.IOError _ GHC.NoSuchThing _ desc _ _) = desc
ioCatcher err = displayException err

prettyException :: String -> SomeException -> String
prettyException msg err =
    let handler name f g e = maybe (g e) ((++) (name ++ " Exception: ") . f) $ fromException e
        catch = foldr ($) displayException
                    [ handler "HTTP" httpCatcher
                    , handler "IO"   ioCatcher ]
     in msg ++ ": " ++ catch err
