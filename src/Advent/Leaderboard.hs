{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Advent.Leaderboard where

import Control.Monad            (liftM2, when)
import Control.Applicative      ((<|>))
import Data.Aeson
import Data.Aeson.Types
import Data.ByteString.Lazy     (ByteString(..))
import Data.HashMap.Strict      (member)
import Data.Map                 (Map(..), elems)
import Data.List                (sortOn)
import Data.Ord                 (Down(..))
import qualified Data.Map as M  (lookup)
import Data.Time.Clock          (secondsToNominalDiffTime)
import Data.Time.Format         (formatTime, defaultTimeLocale)
import Data.Time.Clock.POSIX    (getPOSIXTime)
import Text.Printf              (printf, PrintfArg)

newtype Progress = Progress Int deriving Show

data User
    = User {
        name :: String,
        stars :: Integer,
        localScore :: Integer,
        lastStar :: Integer, -- unix timestamp
        progress :: Map Integer Progress
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
        <*> obj .: "completion_day_level"

instance FromJSON Leaderboard where
    parseJSON = withObject "Leaderboard" $ \obj -> Leaderboard
        <$> obj .: "event"
        <*> (elems <$> (obj .: "members" :: Parser (Map String User)))

parseLeaderboard :: ByteString -> Either String Leaderboard
parseLeaderboard = eitherDecode

printLeaderboard :: (Integral t, PrintfArg t) => (User -> t) -> Leaderboard -> IO ()
printLeaderboard ordering (Leaderboard event participants) = do
    let
        members     = sortOn (Down . ordering) participants
        digits      = ceiling . logBase 10 . fromIntegral
        scoreWidth  = digits . ordering . head $ members
        indexWidth  = digits $ length participants
        nameWidth   = fromIntegral . maximum $ map (length . name) participants
        indexFormat = "\x1b[37m%" ++show indexWidth ++ "d)\x1b[0m "
        columnWidth = indexWidth + scoreWidth + 2
        title       = if length event > columnWidth + 9
                        then take (columnWidth + 6) event ++ "..."
                        else event
        spacing = 25 - max 0 (length title - columnWidth)
        firstRowFormat  = "\x1b[37m%-" ++ show columnWidth ++ "s \x1b[32m%" ++ show spacing ++"s\n"
        secondRowFormat = "\x1b[32m%" ++ show columnWidth ++ "c %s\x1b[39m\n"
        printRow idx user = do
            printf indexFormat idx
            printUser ordering (show scoreWidth) (show nameWidth) user

    printf firstRowFormat title $ replicate 10 '1' ++ replicate 6 '2'
    printf secondRowFormat ' ' . take 25 . drop 1 $ cycle ['0'..'9']
    mapM_ (uncurry printRow) $ zip ([1..] :: [Int]) members


printUser :: PrintfArg t => (User -> t) -> String -> String -> User -> IO ()
printUser scoring scoreWidth nameWidth user@(User name _ _ lastStar progress) = do
    let format = "\x1b[1m%" ++ scoreWidth ++ "d\x1b[0m %s \x1b[1m\x1b[92m%-"
                 ++ nameWidth ++ "s \x1b[0m\x1b[37m"
        stars = maybe "\x1b[90m." star . flip M.lookup progress =<< [1..25]
        star (Progress 1) = "\x1b[37m*"
        star (Progress 2) = "\x1b[93m*"

    printf format (scoring user) stars name
    when (lastStar > 0) $ printf "(%s)" =<< timeSince lastStar
    printf "\x1b[39m\n"


timeSince :: Integral a => a -> IO String
timeSince = fmap (formatTime defaultTimeLocale "%dd%2Hh%2Mm%2Ss") .
   liftM2 (-) getPOSIXTime . return . secondsToNominalDiffTime . fromIntegral
