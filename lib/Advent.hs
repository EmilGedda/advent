{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Advent where

import           Prelude                        hiding (readFile, writeFile, null)

import           Control.Exception              (SomeException, displayException, IOException)
import           Control.Monad                  (liftM2)
import           Control.Monad.Catch            (MonadCatch, fromException)
import           Control.Monad.Reader           (ReaderT)
import           Control.Monad.Except           (ExceptT(..), throwError, lift, MonadError, MonadTrans)
import           Data.Bool                      (bool)
import           Data.ByteString                (ByteString)
import           Data.Time                      (UTCTime)
import           Data.Time.Calendar             (toGregorian)
import           Data.Time.Clock                (getCurrentTime, utctDay, secondsToNominalDiffTime)
import           Data.Time.Clock.POSIX          (getPOSIXTime)
import           System.Directory               (XdgDirectory(XdgConfig), getXdgDirectory, doesFileExist, createDirectoryIfMissing, getAccessTime, removeFile)
import qualified Data.ByteString                (readFile, writeFile)
import qualified Data.ByteString.Char8          as B
import qualified GHC.IO.Exception               as GHC
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

data TimeSince = TimeSince {
                    days    :: Integer,
                    hours   :: Integer,
                    minutes :: Integer,
                    seconds :: Integer
                } deriving (Ord, Eq, Show)

class Monad m => MonadTime m where
    currentYear :: m Integer
    timeSince :: Integral a => a -> m (Maybe TimeSince)

    default currentYear :: Trans MonadTime m Integer
    currentYear = lift currentYear

    default timeSince :: Integral a => a -> Trans MonadTime m (Maybe TimeSince)
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
        (year, month, _) <- toGregorian . utctDay <$> getCurrentTime
        return $ bool year (year - 1) (month < 12)

    timeSince 0 = return Nothing
    timeSince v = do
        posix <- liftM2 (-) getPOSIXTime . return
                 . secondsToNominalDiffTime $ fromIntegral v
        let
            secondsPerHour = 60 * 60
            secondsPerDay  = 24 * secondsPerHour

            timestamp = floor (toRational posix)

            (days, restHours) = timestamp `divMod` secondsPerDay
            (hours, restMins) = restHours `divMod` secondsPerHour
            (mins, secs)      = restMins `divMod` 60

        return . Just $ TimeSince days hours mins secs


instance MonadFS   m => MonadFS   (ExceptT e m)
instance MonadFS   m => MonadFS   (ReaderT e m)
instance MonadTime m => MonadTime (ExceptT e m)
instance MonadTime m => MonadTime (ReaderT r m)
instance MonadHTTP m => MonadHTTP (ExceptT e m)

catch :: (MonadCatch m, MonadError String m) => m a -> String -> m a
catch e str = e `C.catch` (throwError . prettyException str)

-- httpCatcher :: H.HttpException -> String
-- httpCatcher (H.HttpExceptionRequest req content)
--     = case content of -- HTTP Exceptions
--         H.StatusCodeException res _
--              -> "expected 200 OK but got "
--                 ++ show (statusCode . responseStatus $ res)
--                 ++ " "
--                 ++ B.unpack (statusMessage . responseStatus $ res)
--                 ++ " during "
--                 ++ B.unpack (H.method req)
--                 ++ " "
--                 ++ show (H.getUri req)
--
--         H.ConnectionFailure conerr
--             -> flip prettyException conerr
--                 $ "connecting to "
--                 ++ show (H.getUri req)
--                 ++ " failed"
--
--         _ -> show content
--
-- httpCatcher err = displayException err

ioCatcher :: IOException -> String
ioCatcher (GHC.IOError _ GHC.NoSuchThing _ desc _ _) = desc
ioCatcher err = displayException err

prettyException :: String -> SomeException -> String
prettyException msg err =
    let handler name f g e = maybe (g e) ((++) (name ++ " Exception: ") . f) $ fromException e
        catch = foldr ($) displayException
                    [ --handler "HTTP" httpCatcher
                   -- ,
                    handler "IO"   ioCatcher ]
     in msg ++ ": " ++ catch err
