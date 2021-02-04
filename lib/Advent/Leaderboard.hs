{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Advent.Leaderboard where

import           Advent
import           Data.Aeson
import           Data.Aeson.Types
import           Control.Applicative      ((<|>))
import           Control.Monad            (zipWithM)
import           Control.Monad.Except     (liftEither, MonadError)
import           Data.ByteString.Lazy     (ByteString)
import           Data.HashMap.Strict      (member)
import           Data.List                (sortBy, partition)
import           Data.Ord                 (Down(..), comparing)
import           Lens.Micro               ((%~), both)
import           Data.Time.Format         (formatTime, defaultTimeLocale)
import           Text.Printf              (printf, PrintfArg)
import qualified Data.Map                 as M

newtype Progress = Progress { fromProgress :: Int } deriving Show

data User
    = User {
        name :: String,
        stars :: Integer,
        localScore :: Integer,
        lastStar :: Integer, -- unix timestamp, 0 if no star
        userid :: Integer,
        progress :: M.Map Integer Progress
    } deriving Show

data Leaderboard
    = Leaderboard {
        event :: String,
        members :: [User]
    } deriving Show

instance FromJSON Progress where
    parseJSON = withObject "Progress" $
        return . Progress . length . filter id
               . flip map ["1","2"] . flip member

instance FromJSON User where
    parseJSON = withObject "User" $ \obj -> User
        <$> obj .: "name"
        <*> obj .: "stars"
        <*> obj .: "local_score"
        <*> (obj .: "last_star_ts" <|> (read <$> obj .: "last_star_ts"))
        <*> (obj .: "id" <|> (read <$> obj .: "id"))
        <*> obj .: "completion_day_level"

instance FromJSON Leaderboard where
    parseJSON = withObject "Leaderboard" $ \obj -> Leaderboard
        <$> obj .: "event"
        <*> (M.elems <$> (obj .: "members" :: Parser (M.Map String User)))


digits :: Integral a => a -> Int
digits  = (+1) . (floor :: Double -> Int) . logBase 10 . fromIntegral


parseLeaderboard :: MonadError String m => ByteString -> m Leaderboard
parseLeaderboard = liftEither . eitherDecode


prettyLeaderboard :: (Integral t, PrintfArg t, MonadTime m) => (User -> t) -> Leaderboard -> m String
prettyLeaderboard score (Leaderboard event participants) = do
    let
        members = sortBy order participants
        order   = comparing (Down . score) <> recency
        recency a b
                | lastStar a == lastStar b = EQ
                | lastStar a == 0 = GT
                | lastStar b == 0 = LT
                | otherwise = compare (lastStar a) (lastStar b)
        scoreWidth  = digits . score . head $ members
        indexWidth  = digits $ length participants
        nameWidth   = fromIntegral . maximum $ map (length . name) participants
        columnWidth = indexWidth + scoreWidth + 2
        title       = if length event > columnWidth + 9
                        then take (columnWidth + 6) event ++ "..."
                        else event
        spacing         = 25 - max 0 (length title - columnWidth)
        indexFormat     = printf "\x1b[37m%%%dd)\x1b[0m " indexWidth
        firstRowFormat  = printf "\x1b[37m%%-%ds \x1b[32m%%%ds\n" columnWidth spacing
        secondRowFormat = printf "\x1b[32m%%%dc %%s\x1b[39m\n" columnWidth
        printRow idx user = mappend (printf indexFormat idx)
                        <$> prettyUser score scoreWidth nameWidth user

    r <- zipWithM printRow [1 :: Int ..] members

    return $ printf firstRowFormat title (replicate 10 '1' ++ replicate 6 '2')
          <> printf secondRowFormat ' ' (take 25 . drop 1 $ cycle ['0'..'9'])
          <> concat r
          <> "\x1b[0m"


prettyUser :: (MonadTime m, PrintfArg t) => (User -> t) -> Int -> Int -> User -> m String
prettyUser scoring scoreWidth nameWidth user@(User name _ _ lastStar _ progress) = do
    let format = printf "\x1b[1m%%%dd\x1b[0m %%s \x1b[1m\x1b[92m%%-%ds \x1b[0m\x1b[37m" scoreWidth nameWidth
        stars = maybe "\x1b[90m." star . flip M.lookup progress =<< [1..25]
        star (Progress 1) = "\x1b[37m*"
        star (Progress _) = "\x1b[93m*"

    prev <- timeSince lastStar
    return $ printf format (scoring user) stars name
          <> if lastStar > 0
                then printf "(%s)\n" $ formatTime defaultTimeLocale "%dd%2Hh%2Mm%2Ss" prev
                else ""
          <> printf "\x1b[39m\n"


starCount :: User -> (Int, Int)
starCount = (both %~ length) . partition (==1) . map fromProgress . M.elems . progress
