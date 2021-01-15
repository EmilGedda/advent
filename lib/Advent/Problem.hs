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
import           Advent
import           Advent.API
import           Advent.Problem.Util
import           Advent.Problem.Types

import           Control.Monad.Catch                        (MonadCatch)
import           Control.Monad.Except                       (MonadError)
import           Data.ByteString                            (ByteString, stripSuffix)
import           Data.Maybe                                 (fromMaybe)
import           System.FilePath                            ((</>))
import           Text.Printf                                (printf)

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

fetchInput :: (MonadHTTP m, MonadError String m, MonadCatch m, MonadFS m)
    => Integer -> Integer -> m Input
fetchInput year day = do
    dir <- fmap (</> show year </> printf "%02d" day) cacheDir
    createDir dir
    let cache = dir </> "input.txt"
    exists <- hasFile cache
    if exists
       then Input <$> readFile cache
       else do
           i <- input year day
           writeFile cache i
           return $ Input i
