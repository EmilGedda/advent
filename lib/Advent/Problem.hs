{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Advent.Problem (
        module Advent.Problem.Util,
        module Advent.Problem.Types,
        Day(..),
        Year(..),
        Input(..),
        day,
        notSolved,
        fromInput,
        fetchInput
    ) where

import           Prelude hiding                             (readFile, writeFile, lines)
import           Advent.Problem.Util
import           Advent.Problem.Types
import           Advent.API                                 (input, MonadHTTP)
import           Control.Arrow                              ((***))
import           Control.Monad                              (unless, foldM)
import           Control.Monad.Catch                        (MonadCatch)
import           Control.Monad.Except                       (ExceptT, lift, liftIO, MonadIO, MonadError)
import           Data.ByteString                            (ByteString, readFile, writeFile, stripSuffix)
import           Data.ByteString.Char8                      (lines, readInt, readInteger, unpack, pack, split)
import           Data.ByteString.Lazy                       (toStrict)
import           Data.Maybe                                 (fromMaybe, fromJust)
import           Data.List                                  (foldl1')
import           Debug.Trace                                (trace)
import           System.Directory                           (XdgDirectory(XdgConfig), getXdgDirectory
                                                            , doesFileExist, createDirectoryIfMissing)
import           System.FilePath                            ((</>))
import           Text.Printf                                (printf)
import           Data.Attoparsec.ByteString.Char8   hiding  (takeWhile)
import qualified Data.Set                           as      Set
import qualified Data.Vector                        as      V
import qualified Data.Vector.Unboxed                as      UV

newtype Input = Input ByteString deriving Show

data Day = forall a b c. (Parseable a, ToString b, ToString c)
         => Day {
           number :: Integer,
           partOne :: a -> b,
           partTwo :: a -> c
        }

day :: (Parseable a, ToString b, ToString c)
        => Integer -> (a -> b) -> (a -> c) -> Day
day = Day

data Year = Year {
                year :: Integer,
                days :: [Day]
             }


notSolved :: Parseable a => a -> String
notSolved = const "Not solved"

fromInput :: Input -> ByteString
fromInput (Input str) = fromMaybe str $ stripSuffix "\n" str

fetchInput :: (MonadHTTP m, MonadIO m, MonadError String m, MonadCatch m)
    => Integer -> Integer -> m Input
fetchInput year day = do
    dir <- liftIO $ (</> show year </> printf "%02d" day) <$> getXdgDirectory XdgConfig "AdventOfCode"
    liftIO $ createDirectoryIfMissing True dir
    let cache = dir </> "input.txt"
    hasFile <- liftIO $ doesFileExist cache
    input <- if hasFile then liftIO (readFile cache) else input year day
    unless hasFile (liftIO $ writeFile cache input)
    return $ Input input

