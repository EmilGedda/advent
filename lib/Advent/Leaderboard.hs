{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
module Advent.Leaderboard where

import           Advent
import           Advent.Problem.Util        hiding (fold, both)

import           Data.Aeson.Micro
import           Control.Monad              (zipWithM)
import           Control.Monad.Except       (MonadError, throwError)
import           Data.Foldable              (fold)
import           Data.List                  (sortBy, partition)
import           Data.Maybe                 (catMaybes)
import           Data.Ord                   (Down(..), comparing)
import           Lens.Micro                 ((%~), both)
import           Text.Printf                (printf, PrintfArg)
import qualified Data.Map                   as M
import qualified Data.ByteString.Char8      as B
import qualified Data.Text                  as T
import qualified Data.Text.Read             as TR


newtype Progress = Progress { fromProgress :: Int }

data User
    = User {
        name :: String,
        stars :: Integer,
        localScore :: Integer,
        lastStar :: Integer,
        userid :: Integer,
        progress :: M.Map Integer Progress
    }

data Leaderboard
    = Leaderboard {
        event :: String,
        members :: [User]
    }

instance FromJSON Progress where
    parseJSON = withObject "Progress" $
        return . Progress . length . filter id
               . flip map ["1","2"] . flip M.member

instance FromJSON User where
    parseJSON = withObject "User" $ \v -> User
        <$> (T.unpack <$> v .: "name")
        <*> v .: "stars"
        <*> v .: "local_score"
        <*> (parseInt <$> v .: "last_star_ts")
        <*> (read . T.unpack <$> v .: "id")
        <*> (M.mapKeys readInt <$> v .: "completion_day_level")

parseInt (String txt) = readInt txt
parseInt (Number num) = round num

readInt :: T.Text -> Integer
readInt = fst . fromRight . TR.decimal

instance FromJSON Leaderboard where
    parseJSON = withObject "Leaderboard" $ \obj -> Leaderboard
        <$> (T.unpack <$> obj .: "event")
        <*> (M.elems <$> (obj .: "members" :: Parser (M.Map T.Text User)))


digits :: Integral a => a -> Int
digits  = (+1) . (floor :: Double -> Int) . logBase 10 . fromIntegral

parseLeaderboard :: MonadError String m => B.ByteString -> m Leaderboard
parseLeaderboard = maybe (throwError "Failed to parse leaderboard json") return . decodeStrict


prettyLeaderboard :: (Integral t, PrintfArg t, MonadTime m) => (User -> t) -> Leaderboard -> m String
prettyLeaderboard score (Leaderboard event participants) = do
    times <- mapM (timeSince . lastStar) participants

    let
        toInt :: Integral a => a -> Int
        toInt = fromIntegral

        members = sortBy order participants
        order   = comparing (Down . score) <> comparing lastStar
        scoreWidth  = fromIntegral . digits . score . head $ members
        indexWidth  = fromIntegral . digits $ length participants
        nameWidth   = fromIntegral . maximum $ map (length . name) participants
        columnWidth = indexWidth + scoreWidth + 2
        timeWidth   = maximum . map (length . toDigits . days) $ catMaybes times
        title       = if length event > columnWidth + 9
                        then take (columnWidth + 6) event <> "..."
                        else event
        spacing         = 25 - max 0 (length title - toInt columnWidth)
        indexFormat     = printf "\x1b[37m%%%dd)\x1b[0m " indexWidth
        firstRowFormat  = printf "\x1b[37m%%-%ds \x1b[32m%%%ds\n" columnWidth spacing
        secondRowFormat = printf "\x1b[32m%%%dc %%s\x1b[39m\n" columnWidth
        printRow w i user = mappend (printf indexFormat i)
                        <$> prettyUser score (toInt scoreWidth) nameWidth w user

    r <- zipWithM (printRow timeWidth) [1 :: Int ..] members

    return $ printf firstRowFormat title (replicate 10 '1' ++ replicate 6 '2')
          <> printf secondRowFormat ' ' (take 25 . drop 1 $ cycle ['0'..'9'])
          <> concat r
          <> "\x1b[0m"


prettyUser :: (MonadTime m, PrintfArg t) => (User -> t) -> Int -> Int -> Int -> User -> m String
prettyUser scoring scoreWidth nameWidth timeWidth user@(User name _ _ lastStar _ progress) = do
    let format = printf "\x1b[1m%%%dd\x1b[0m %%s \x1b[1m\x1b[92m%%-%ds \x1b[0m\x1b[37m" scoreWidth nameWidth
        stars = maybe "\x1b[90m." star . flip M.lookup progress =<< [1..25]
        star (Progress 1) = "\x1b[37m*"
        star (Progress _) = "\x1b[93m*"

    timestamp <- timeSince lastStar

    return $ printf format (scoring user) stars name
          <> fold (prettyTime timeWidth <$> timestamp)
          <> printf "\x1b[39m\n"


prettyTime :: Int -> TimeSince -> String
prettyTime width TimeSince{days, hours, minutes, seconds} =
    let format = "(%0" ++ show width ++ "dd%02dh%02dm%02ds)"
    in printf format days hours minutes seconds


starCount :: User -> (Int, Int)
starCount = (both %~ length) . partition (==1) . map fromProgress . M.elems . progress
