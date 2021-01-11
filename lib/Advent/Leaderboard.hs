{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Advent.Leaderboard where

import Control.Monad            (liftM2, when)
import Control.Applicative      ((<|>))
import Control.Lens             ((%~), both)
import Data.Aeson
import Data.Aeson.Types
import Data.ByteString.Lazy     (ByteString)
import Data.HashMap.Strict      (member)
import Data.Map                 (Map, elems)
import Data.List                (sortBy, partition)
import Data.Ord                 (Down(..), comparing)
import qualified Data.Map as M  (lookup)
import Data.Time.Clock          (secondsToNominalDiffTime)
import Data.Time.Format         (formatTime, defaultTimeLocale)
import Data.Time.Clock.POSIX    (getPOSIXTime)
import Text.Printf              (printf, PrintfArg)

newtype Progress = Progress { fromProgress :: Int } deriving Show

data User
    = User {
        name :: String,
        stars :: Integer,
        localScore :: Integer,
        lastStar :: Integer, -- unix timestamp, 0 if no star
        userid :: Integer,
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
        <*> (obj .: "id" <|> (read <$> obj .: "id"))
        <*> obj .: "completion_day_level"

instance FromJSON Leaderboard where
    parseJSON = withObject "Leaderboard" $ \obj -> Leaderboard
        <$> obj .: "event"
        <*> (elems <$> (obj .: "members" :: Parser (Map String User)))

digits :: Integral a => a -> Int
digits  = (+1) . (floor :: Double -> Int) . logBase 10 . fromIntegral

parseLeaderboard :: ByteString -> Either String Leaderboard
parseLeaderboard = eitherDecode

printLeaderboard :: (Integral t, PrintfArg t) => (User -> t) -> Leaderboard -> IO ()
printLeaderboard score (Leaderboard event participants) = do
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
        printRow idx user = do
            printf indexFormat idx
            printUser score scoreWidth nameWidth user

    printf firstRowFormat title $ replicate 10 '1' ++ replicate 6 '2'
    printf secondRowFormat ' ' . take 25 . drop 1 $ cycle ['0'..'9']
    mapM_ (uncurry printRow) $ zip ([1..] :: [Int]) members


printUser :: PrintfArg t => (User -> t) -> Int -> Int -> User -> IO ()
printUser scoring scoreWidth nameWidth user@(User name _ _ lastStar _ progress) = do
    let format = printf "\x1b[1m%%%dd\x1b[0m %%s \x1b[1m\x1b[92m%%-%ds \x1b[0m\x1b[37m" scoreWidth nameWidth
        stars = maybe "\x1b[90m." star . flip M.lookup progress =<< [1..25]
        star (Progress 1) = "\x1b[37m*"
        star (Progress _) = "\x1b[93m*"

    printf format (scoring user) stars name
    when (lastStar > 0) $ printf "(%s)" =<< timeSince lastStar
    printf "\x1b[39m\n"


timeSince :: Integral a => a -> IO String
timeSince = fmap (formatTime defaultTimeLocale "%dd%2Hh%2Mm%2Ss") .
   liftM2 (-) getPOSIXTime . return . secondsToNominalDiffTime . fromIntegral

starCount :: User -> (Int, Int)
starCount = (both %~ length) . partition (==1) . map fromProgress . elems . progress
