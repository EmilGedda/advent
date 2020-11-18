{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Advent.API.Leaderboard where

import Data.Aeson
import Data.Aeson.Types
import Data.ByteString.Lazy     (ByteString(..))
import Data.HashMap.Strict      (member)
import Data.Map                 (Map(..), elems)
import Data.List                (sortBy)
import Data.Ord                 (comparing, Down(..))
import qualified Data.Map as M  (lookup)
import Data.Maybe               (maybe, isJust)
import Data.Time.Clock          (secondsToNominalDiffTime)
import Data.Time.Format         (formatTime, defaultTimeLocale)
import Data.Time.Clock.POSIX    (getPOSIXTime, POSIXTime(..))
import Text.Printf              (printf)

data Progress = Progress Int deriving Show

data User
    = User {
        name :: String,
        stars :: Integer,
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
        <*> (read <$> obj .: "last_star_ts")
        <*> obj .: "completion_day_level"

instance FromJSON Leaderboard where
    parseJSON = withObject "Leaderboard" $ \obj -> Leaderboard
        <$> obj .: "event"
        <*> (elems <$> (obj .: "members" :: Parser (Map String User)))

parseLeaderboard :: ByteString -> Either String Leaderboard
parseLeaderboard = eitherDecode


printLeaderboard :: Leaderboard -> IO ()
printLeaderboard (Leaderboard event participants) = do
    let
        members = sortBy (comparing (Down . stars)) participants
        digits  = ceiling . logBase 10 . fromIntegral
        scoreWidth = digits . stars . head $ members
        indexWidth = digits $ length participants
        nameWidth  = fromIntegral . maximum $ map (length . name) participants
        indexFormat = "\x1b[37m%" ++show indexWidth ++ "d)\x1b[0m "
        timeFormat  = '%':show nameWidth ++ "s (%s)"
        columnWidth = indexWidth + scoreWidth + 2
        spacing = 25 - max 0 (length event - columnWidth)
        firstHeaderFormat = "\x1b[37m%-" ++ show columnWidth ++ "s \x1b[32m%" ++ show spacing ++"s\n"
        secondHeaderFormat = "\x1b[32m%" ++ show columnWidth ++ "c %s\x1b[39m\n"
        printRow idx user = do
            printf indexFormat idx
            printUser scoreWidth nameWidth user

    printf firstHeaderFormat event (replicate 10 '1' ++ replicate 6 '2')
    printf secondHeaderFormat ' ' (take 25 . drop 1 $ cycle ['0'..'9'])
    mapM_ (uncurry printRow) $ zip ([1..] :: [Int]) members


printUser :: Integer -> Integer -> User -> IO ()
printUser scoreWidth nameWidth (User name totalStars lastStar progress) =
    let format = "\x1b[1m%" ++ show scoreWidth ++ "d\x1b[0m %s \x1b[1m\x1b[92m%-"
                 ++ show nameWidth ++ "s \x1b[0m\x1b[37m(%s)\x1b[39m\n"
        stars = maybe "\x1b[90m." star . flip M.lookup progress =<<  [1..25]
        star (Progress 1) = "\x1b[37m*"
        star (Progress 2) = "\x1b[93m*"
    in printf format totalStars stars name =<< timeSince lastStar


timeSince :: Integer -> IO String
timeSince time = do
    current <- getPOSIXTime
    let diff = current - (secondsToNominalDiffTime $ fromIntegral time)
    return $ formatTime defaultTimeLocale "%dd%2Hh%2Mm%2Ss" diff

testJson :: ByteString
testJson = "{\"owner_id\":\"409260\",\"event\":\"2019\",\"members\":{\"571183\":{\"name\":\"Emil Persson\",\"completion_day_level\":{\"2\":{\"2\":{\"get_star_ts\":\"1575301692\"},\"1\":{\"get_star_ts\":\"1575277588\"}},\"1\":{\"1\":{\"get_star_ts\":\"1575194952\"},\"2\":{\"get_star_ts\":\"1575199028\"}}},\"local_score\":21,\"id\":\"571183\",\"global_score\":0,\"last_star_ts\":\"1575301692\",\"stars\":4},\"563609\":{\"last_star_ts\":\"1576530957\",\"stars\":16,\"name\":\"LukasSzerszen\",\"completion_day_level\":{\"4\":{\"1\":{\"get_star_ts\":\"1575473234\"},\"2\":{\"get_star_ts\":\"1575474571\"}},\"1\":{\"1\":{\"get_star_ts\":\"1575204314\"},\"2\":{\"get_star_ts\":\"1575207428\"}},\"8\":{\"1\":{\"get_star_ts\":\"1575893627\"},\"2\":{\"get_star_ts\":\"1575981307\"}},\"3\":{\"2\":{\"get_star_ts\":\"1575466968\"},\"1\":{\"get_star_ts\":\"1575385892\"}},\"5\":{\"2\":{\"get_star_ts\":\"1575722996\"},\"1\":{\"get_star_ts\":\"1575670779\"}},\"10\":{\"2\":{\"get_star_ts\":\"1576530957\"},\"1\":{\"get_star_ts\":\"1576354084\"}},\"2\":{\"1\":{\"get_star_ts\":\"1575302857\"},\"2\":{\"get_star_ts\":\"1575305369\"}},\"6\":{\"2\":{\"get_star_ts\":\"1576347500\"},\"1\":{\"get_star_ts\":\"1575732216\"}}},\"global_score\":0,\"local_score\":56,\"id\":\"563609\"},\"694295\":{\"last_star_ts\":\"1576751088\",\"stars\":22,\"completion_day_level\":{\"8\":{\"2\":{\"get_star_ts\":\"1575997403\"},\"1\":{\"get_star_ts\":\"1575993772\"}},\"3\":{\"1\":{\"get_star_ts\":\"1575473353\"},\"2\":{\"get_star_ts\":\"1575476551\"}},\"4\":{\"1\":{\"get_star_ts\":\"1575482893\"},\"2\":{\"get_star_ts\":\"1575493149\"}},\"11\":{\"1\":{\"get_star_ts\":\"1576678300\"},\"2\":{\"get_star_ts\":\"1576751088\"}},\"1\":{\"1\":{\"get_star_ts\":\"1575281462\"},\"2\":{\"get_star_ts\":\"1575282494\"}},\"10\":{\"2\":{\"get_star_ts\":\"1576077483\"},\"1\":{\"get_star_ts\":\"1576058522\"}},\"5\":{\"1\":{\"get_star_ts\":\"1575649755\"},\"2\":{\"get_star_ts\":\"1575675386\"}},\"9\":{\"1\":{\"get_star_ts\":\"1576021730\"},\"2\":{\"get_star_ts\":\"1576021887\"}},\"7\":{\"2\":{\"get_star_ts\":\"1575910033\"},\"1\":{\"get_star_ts\":\"1575894547\"}},\"2\":{\"2\":{\"get_star_ts\":\"1575302945\"},\"1\":{\"get_star_ts\":\"1575295121\"}},\"6\":{\"2\":{\"get_star_ts\":\"1575884760\"},\"1\":{\"get_star_ts\":\"1575680116\"}}},\"name\":\"kennethrunnman\",\"global_score\":0,\"local_score\":82,\"id\":\"694295\"},\"56918\":{\"completion_day_level\":{\"9\":{\"2\":{\"get_star_ts\":\"1575929566\"},\"1\":{\"get_star_ts\":\"1575929501\"}},\"11\":{\"1\":{\"get_star_ts\":\"1576083886\"},\"2\":{\"get_star_ts\":\"1576087514\"}},\"1\":{\"2\":{\"get_star_ts\":\"1575207025\"},\"1\":{\"get_star_ts\":\"1575203402\"}},\"16\":{\"2\":{\"get_star_ts\":\"1577095497\"},\"1\":{\"get_star_ts\":\"1576585973\"}},\"10\":{\"1\":{\"get_star_ts\":\"1575979634\"},\"2\":{\"get_star_ts\":\"1576005317\"}},\"6\":{\"2\":{\"get_star_ts\":\"1575640018\"},\"1\":{\"get_star_ts\":\"1575635693\"}},\"17\":{\"1\":{\"get_star_ts\":\"1576941359\"},\"2\":{\"get_star_ts\":\"1577056091\"}},\"7\":{\"1\":{\"get_star_ts\":\"1575717640\"},\"2\":{\"get_star_ts\":\"1575748249\"}},\"15\":{\"1\":{\"get_star_ts\":\"1577446753\"},\"2\":{\"get_star_ts\":\"1577449758\"}},\"4\":{\"1\":{\"get_star_ts\":\"1575436515\"},\"2\":{\"get_star_ts\":\"1575436944\"}},\"14\":{\"1\":{\"get_star_ts\":\"1577470350\"},\"2\":{\"get_star_ts\":\"1577472276\"}},\"8\":{\"2\":{\"get_star_ts\":\"1575798578\"},\"1\":{\"get_star_ts\":\"1575796684\"}},\"3\":{\"2\":{\"get_star_ts\":\"1575355181\"},\"1\":{\"get_star_ts\":\"1575353833\"}},\"5\":{\"2\":{\"get_star_ts\":\"1575594698\"},\"1\":{\"get_star_ts\":\"1575587495\"}},\"2\":{\"2\":{\"get_star_ts\":\"1575264585\"},\"1\":{\"get_star_ts\":\"1575264217\"}},\"19\":{\"2\":{\"get_star_ts\":\"1577535163\"},\"1\":{\"get_star_ts\":\"1577484483\"}},\"12\":{\"2\":{\"get_star_ts\":\"1576583969\"},\"1\":{\"get_star_ts\":\"1576177730\"}},\"13\":{\"1\":{\"get_star_ts\":\"1576249646\"},\"2\":{\"get_star_ts\":\"1576365813\"}}},\"name\":\"Emil Gedda\",\"local_score\":212,\"id\":\"56918\",\"global_score\":0,\"last_star_ts\":\"1577535163\",\"stars\":36},\"665097\":{\"last_star_ts\":\"1575276959\",\"stars\":3,\"name\":\"miht\",\"completion_day_level\":{\"1\":{\"2\":{\"get_star_ts\":\"1575221424\"},\"1\":{\"get_star_ts\":\"1575221140\"}},\"2\":{\"1\":{\"get_star_ts\":\"1575276959\"}}},\"global_score\":0,\"local_score\":9,\"id\":\"665097\"},\"409260\":{\"completion_day_level\":{\"5\":{\"2\":{\"get_star_ts\":\"1575598776\"},\"1\":{\"get_star_ts\":\"1575597077\"}},\"10\":{\"1\":{\"get_star_ts\":\"1576020763\"}},\"4\":{\"1\":{\"get_star_ts\":\"1575468576\"},\"2\":{\"get_star_ts\":\"1575495597\"}},\"11\":{\"1\":{\"get_star_ts\":\"1576290501\"}},\"1\":{\"1\":{\"get_star_ts\":\"1575196803\"},\"2\":{\"get_star_ts\":\"1575199938\"}},\"3\":{\"2\":{\"get_star_ts\":\"1575392881\"},\"1\":{\"get_star_ts\":\"1575391332\"}},\"8\":{\"1\":{\"get_star_ts\":\"1575818845\"},\"2\":{\"get_star_ts\":\"1575823758\"}},\"7\":{\"1\":{\"get_star_ts\":\"1575971473\"}},\"9\":{\"1\":{\"get_star_ts\":\"1576060505\"},\"2\":{\"get_star_ts\":\"1576060968\"}},\"13\":{\"1\":{\"get_star_ts\":\"1576356538\"}},\"6\":{\"1\":{\"get_star_ts\":\"1575669760\"},\"2\":{\"get_star_ts\":\"1575851628\"}},\"12\":{\"1\":{\"get_star_ts\":\"1576283116\"}},\"2\":{\"2\":{\"get_star_ts\":\"1575311400\"},\"1\":{\"get_star_ts\":\"1575310175\"}}},\"name\":\"Anders Eriksson\",\"global_score\":0,\"local_score\":92,\"id\":\"409260\",\"last_star_ts\":\"1576356538\",\"stars\":21}}}"
