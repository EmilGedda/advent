{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Advent.Problem where

import Prelude hiding               (readFile, writeFile)
import Advent.API                   (get, input)
import Control.Monad                (unless)
import Data.ByteString              (ByteString, readFile, writeFile, stripSuffix)
import Data.ByteString.Char8        (split, readInt, readInteger, unpack)
import Data.ByteString.Lazy         (toStrict)
import Data.Maybe                   (fromMaybe, fromJust)
import Control.Monad.Except         (ExceptT, lift)
import System.Directory             (XdgDirectory(XdgConfig), getXdgDirectory, doesFileExist, createDirectoryIfMissing)
import System.FilePath              ((</>))
import Text.Printf                  (printf)


class Parseable a where
    parseInput :: ByteString -> a

instance Parseable Double where
    parseInput = read . unpack

instance Parseable Integer where
    parseInput = fst . fromJust . readInteger

instance Parseable Int where
    parseInput = fst . fromJust . readInt

instance Parseable a => Parseable [a] where
    parseInput = map parseInput . split '\n'


class ToString a where
    solution :: a -> String

instance Show a => ToString a where
    solution = show

instance {-# OVERLAPPING #-} ToString Char where
    solution = return

instance {-# OVERLAPPING #-} ToString String where
    solution = id


newtype Input = Input ByteString deriving Show

data Day = Day {
               number :: Integer,
               partOne :: Input -> String,
               partTwo :: Input -> String
            }

-- TODO: move to Util
every :: Int -> [a] -> [a]
every n = map head . takeWhile (not . null) . iterate (drop n)

fromRight (Right r) = r

notSolved :: Int -> String
notSolved = const "Not solved"

day
  :: (Parseable a, ToString b, Parseable c, ToString d) =>
     Integer -> (a -> b) -> (c -> d) -> Day
day number partOne partTwo = Day number (wrap partOne) (wrap partTwo)
    where wrap part = solution . part . parseInput . fromInput

fromInput :: Input -> ByteString
fromInput (Input str) = fromMaybe str $ stripSuffix "\n" str

fetchInput :: Integer -> Integer -> ExceptT String IO Input
fetchInput year day = do
    dir <- lift $ (</> show year </> printf "%02d" day) <$> getXdgDirectory XdgConfig "AdventOfCode"
    lift $ createDirectoryIfMissing True dir
    let cache = dir </> "input.txt"
    hasFile <- lift $ doesFileExist cache
    input <- if hasFile then lift (readFile cache) else toStrict <$> get (input year day)
    unless hasFile (lift $ writeFile cache input)
    return $ Input input

