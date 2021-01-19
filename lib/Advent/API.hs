{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ConstraintKinds #-}

module Advent.API where

import           Advent
import           Advent.Problem.Util
import           Advent.Leaderboard

import           Control.Applicative            ((<|>))
import           Control.Monad                  (when, unless, (<=<))
import           Control.Monad.Catch            (MonadCatch)
import           Control.Monad.Except           (throwError, MonadError)
import           Control.Monad.Reader           (ReaderT(..), runReaderT, MonadIO, liftIO)
import           Data.ByteString                (ByteString, null, stripSuffix)
import           Data.ByteString.Char8          (readInteger, pack)
import           Data.ByteString.Lazy           (fromStrict)
import           Data.List                      (find)
import           Data.Maybe                     (fromMaybe, listToMaybe)
import           Data.Time                      (UTCTime, parseTimeOrError, defaultTimeLocale)
import           Network.HTTP.Client            (CookieJar, Cookie(..), createCookieJar)
import           Network.HTTP.Client.OpenSSL    (opensslManagerSettings, defaultMakeContext, defaultOpenSSLSettings)
import           Prelude hiding                 (readFile, writeFile, null)
import           System.FilePath                ((</>))
import           Text.Printf                    (printf)
import           Text.Regex.TDFA                ((=~))
import           Text.Regex.TDFA.ByteString     ()
import qualified Network.Wreq.Session       as  S

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


session :: (MonadIO m, MonadError String m) => ByteString -> m S.Session
session = liftIO . flip S.newSessionControl tls . Just . sessionCookie
    where tls = opensslManagerSettings $ defaultMakeContext defaultOpenSSLSettings


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
           maybe (deleteFile cache >> currentUserID) (return . fst)
             . (fromBool (const $ token <= latest) <=< readInteger)
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
    let anonuser  = regexMatchGroups "anonymous user #([0-9]+)" str
        codehover = regexMatchGroups "<code>([0-9]+)" str
        readID list = fmap fst . readInteger =<< listToMaybe list

    in maybe (throwError "Unable to find user id") return
             $ readID anonuser <|> readID codehover


regexMatchGroups :: ByteString -> ByteString -> [ByteString]
regexMatchGroups regex str = groups
    where (_, _, _, groups) = str =~ regex
            :: (ByteString, ByteString, ByteString, [ByteString])


leaderboard :: (MonadError String m, MonadHTTP m, MonadCatch m)
            => Integer -> Integer -> m Leaderboard
leaderboard year id = parseLeaderboard . fromStrict =<< fetch url
                      `catch` "Unable to fetch leaderboard"
    where url = printf "/%d/leaderboard/private/view/%d.json" year id
