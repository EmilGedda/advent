{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Solutions.Y2020.D20 (day20) where

import           Advent.Problem
import           Data.List                          (transpose, find)
import           Data.Maybe                         (catMaybes, mapMaybe)
import           Data.Attoparsec.ByteString.Char8   hiding (take, count)
import qualified Data.Map.Strict                    as M
import qualified Data.HashMap.Strict                as H

data Square a = Square {
                num :: Int,
                contents :: a
            } deriving (Show, Functor)

type Tile = Square [String]

newtype Tiles = Tiles [Tile]

instance Eq Tile where
    (==) (Square a c) (Square b d) = a == b && c == d

instance Ord Tile where
    compare (Square a _) (Square b _) = compare a b

data Connection = Connection {
                above :: Maybe Tile,
                right :: Maybe Tile,
                below :: Maybe Tile,
                left  :: Maybe Tile
            } deriving Show

tile = Square
    <$> ("Tile " *> decimal <* ":\n")
    <*> (lines <$> manyTill' anyChar "\n\n")

instance Parseable Tile where
    parseInput = attoparse tile

instance Parseable Tiles where
    parseInput = Tiles . fromRight . eitherResult
               . flip feed "\n" . parse (many1 tile)

day20 :: Day 20
day20 = day (product . map num . corners . puzzle) notSolved

puzzle :: Tiles -> M.Map Tile Connection
puzzle (Tiles ts) =
    let
        tiles       = orientations =<< ts
        hashmap f   = foldr (\t -> H.insertWith (++) (f t) [t])  H.empty tiles
        match m f t = find (\x -> num x /= num t) =<< m H.!? f t

        tops    = hashmap topside
        rights  = hashmap rightside
        lefts   = hashmap leftside
        bottoms = hashmap bottom

        connect tile
            = Connection
                (match bottoms topside   tile)
                (match lefts   rightside tile)
                (match tops    bottom    tile)
                (match rights  leftside  tile)

        invalid (Connection Nothing Nothing Nothing Nothing) = True
        invalid _ = False

     in M.fromList . filter (not . invalid . snd) $ zip tiles (map connect tiles)

reorder :: M.Map Tile Connection -> M.Map Tile Connection
reorder m = m

connections :: Connection -> [String]
connections c
  = mapMaybe ($ c) [ fmap bottom    . above
                   , fmap leftside  . right
                   , fmap topside   . below
                   , fmap rightside . left ]

topside :: Tile -> String
topside (Square _ (x:_)) = x

bottom :: Tile -> String
bottom (Square _ t) = last t

rightside :: Tile -> String
rightside = bottom  . fmap transpose

leftside :: Tile -> String
leftside  = topside . fmap transpose

corners :: M.Map Tile Connection -> [Tile]
corners = M.keys . M.filter ((==2) . length . connections)
    where connections (Connection a r b l) = catMaybes [a, r, b, l]

orientations :: Tile -> [Tile]
orientations t = mirror =<< rotate t
    where rotate = take 4 . iterate (fmap $ reverse . transpose)
          mirror x = [x, fmap reverse x]
