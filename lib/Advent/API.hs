{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ConstraintKinds #-}

module Advent.API where

import           Prelude hiding                 (readFile, writeFile, null)

import           Advent
import           Advent.Problem.Util
import           Advent.Leaderboard

import           Control.Applicative            ((<|>))
import           Control.Monad                  (when, unless, (<=<))
import           Control.Monad.Catch            (MonadCatch)
import           Control.Monad.Except           (throwError, MonadError)
import           Control.Monad.Reader           (ReaderT(..), runReaderT, MonadIO, liftIO, ask)
import           Data.ByteString                (ByteString, null, stripSuffix)
import           Data.List                      (find)
import           Data.Maybe                     (fromMaybe)
import           Network.Curl                   (curlGetString_, CurlOption(..))
import           System.FilePath                ((</>))
import           Text.Printf                    (printf)
import qualified Data.ByteString.Char8          as B

newtype NetworkEnv = NetworkEnv String

instance MonadIO m => MonadHTTP (ReaderT NetworkEnv m) where
    httpGet url = do
        NetworkEnv cookie <- ask
        (_, res) <- liftIO $ curlGetString_ url [CurlCookie $ "session=" ++ cookie]
        return res

tokenFile :: MonadFS m => m FilePath
tokenFile = (</> "session-token.txt") <$> cacheDir

getSessionToken :: (MonadFS m, MonadError String m, MonadCatch m) => m ByteString
getSessionToken = flip catch "Unable to read session token" $ do
    file   <- tokenFile
    exists <- hasFile file
    unless exists . throwError $ "No session token file found: " ++ file
    token  <- readFile file
    when (null token) $ throwError "Empty session token file"
    return . fromMaybe token $ stripSuffix "\n" token


newNetworkEnv :: (MonadFS m, MonadError String m, MonadCatch m, MonadIO m)
              => m NetworkEnv
newNetworkEnv = NetworkEnv . B.unpack <$> getSessionToken


runNetworkEnv :: (MonadFS m, MonadError String m, MonadCatch m, MonadIO m)
              => ReaderT NetworkEnv m b -> m b
runNetworkEnv r = runReaderT r =<< newNetworkEnv


fetch :: (MonadError String m, MonadHTTP m) => String -> m ByteString
fetch url = httpGet . (++ url) =<< baseURL


input :: (MonadError String m, MonadHTTP m, MonadCatch m)
    => Integer -> Integer -> m ByteString
input year day = fetch url `catch` "Unable to fetch input"
    where url = printf "/%d/day/%d/input" year day


currentUserID :: (MonadError String m, MonadFS m, MonadHTTP m, MonadTime m, MonadCatch m)
              => m Integer
currentUserID = do
    cache <- (</> "user.txt") <$> cacheDir
    exist <- hasFile cache
    if not exist
       then do
           id <- fetchUserID
           writeFile cache . B.pack $ show id
           return id
       else do
           latest <- accessTime cache
           token <- accessTime =<< tokenFile
           maybe (deleteFile cache >> currentUserID) (return . fst)
             . (fromBool (const $ token <= latest) <=< B.readInteger)
             =<< readFile cache


fetchUserID :: (MonadError String m, MonadFS m, MonadHTTP m, MonadTime m, MonadCatch m) => m Integer
fetchUserID = findID =<< fetch "/settings" `catch` "Unable to fetch user id"


currentUser :: (MonadError String m, MonadFS m, MonadHTTP m, MonadTime m, MonadCatch m) => m User
currentUser = do
    id <- currentUserID
    year <- currentYear
    maybe (throwError "Unable to find user") return
        . find ((id ==) . userid)
        . members =<< leaderboard year id


findID :: MonadError String m => ByteString -> m Integer
findID str =
    let after s = B.drop (B.length s) . snd . B.breakSubstring s
        anonuser  = after "anonymous user #" str
        codehover = after "<code>" str
        readID = fmap fst . B.readInteger

    in maybe (throwError "Unable to find user id") return
             $ readID anonuser <|> readID codehover

leaderboard :: (MonadError String m, MonadHTTP m, MonadCatch m)
            => Integer -> Integer -> m Leaderboard
leaderboard year id = parseLeaderboard =<< fetch url
                      `catch` "Unable to fetch leaderboard"
    where url = printf "/%d/leaderboard/private/view/%d.json" year id
