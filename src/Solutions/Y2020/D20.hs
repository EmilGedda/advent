{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module Solutions.Y2020.D20 where

import Advent.Problem hiding (Coord)
import Control.Monad                           (guard)
import Data.Attoparsec.ByteString.Char8 hiding (take, count)
import Data.Bits                        hiding (rotate)
import Data.IntMap.Strict                      (IntMap, fromListWith, elems, (!))

import qualified Data.Map as M
import qualified Data.Set as S

data Tile a = Tile { getID :: Int, getGrid :: [a] }
    deriving (Functor, Eq, Show, Generic, NFData)

newtype Tiles = Tiles { tiles :: [Tile String] } deriving (Generic, NFData)

data Coord = C !Int !Int
    deriving (Eq, Ord, Show)


parseTile :: Parser (Tile String)
parseTile = Tile <$>
    ("Tile " *> decimal <* ":\n")
    <*> (lines <$> manyTill' anyChar "\n\n")

instance Parseable Tiles where
    parseInput = Tiles . fromRight . eitherResult
               . flip feed "\n" . parse (many1 parseTile)


day20 :: Day 20
day20 = day (corners . layImage) (countSnakes . removeBorders . layImage)


layImage :: Tiles -> M.Map Coord (Tile Coord)
layImage = (placeImage <*> firstTile)
    . edgeMap
    . map (liftTile toPicture)
    . tiles

liftTile :: ([a] -> [b]) -> Tile a -> Tile b
liftTile f (Tile id contents) = Tile id (f contents)

toPicture :: [String] -> [Coord]
toPicture tile = do
    (y, row) <- zip [0..] tile
    (x, '#') <- zip [0..] row
    return $ C y x

invert :: Coord -> Coord
invert (C y x) = C x y

add :: Coord -> Coord -> Coord
add (C y x) (C y' x') = C (y + y') (x + x')

row :: Coord -> Int
row (C row _) = row

above, left :: Coord -> Coord
above (C y x) = C (y-1)  x
left  (C y x) = C  y    (x-1)

turnRight :: Coord -> Coord
turnRight (C y x) = C x (-y)

rotate :: [Coord] -> [Coord]
rotate coords = map (add (C 0 n) . turnRight) coords
    where n = maximum $ map row coords

reorient :: [Coord] -> [[Coord]]
reorient xs = do
    xs' <- take 4 (iterate rotate xs)
    [xs', map invert xs']

edgeMap :: [Tile Coord] -> IntMap [Tile Coord]
edgeMap tiles = fromListWith (++) $ do
    Tile id coords <- tiles
    permutations <- reorient coords
    return (leftIdx permutations, [Tile id permutations])

firstTile :: IntMap [Tile Coord] -> Tile Coord
firstTile edges = head $ do
    x:xs <- elems edges
    guard . same $ (x:xs)
    guard . same $ edges ! topIdx (getGrid x)
    return x

corners :: M.Map Coord (Tile Coord) -> Int
corners image = product $ do
    y <- [0, 11]
    x <- [0, 11]
    return . getID $ image M.! C y x

placeImage :: IntMap [Tile Coord] -> Tile Coord -> M.Map Coord (Tile Coord)
placeImage edges corner = board
    where
        board = M.fromList $ (,) <*> pickTile <$> (C <$> [0..11] <*> [0..11])

        pickTile coord
            | coord == C 0 0 = corner

            | row coord == 0 = head $ do
                let Tile id' adj = board M.! left coord
                Tile id coords <- edges ! rightIdx adj
                guard $ id' /= id
                return $ Tile id coords

            | otherwise = head $ do
                let Tile id' adj = board M.! above coord
                Tile id coords <- edges ! bottomIdx adj
                guard $ id' /= id
                return $ Tile id (rotate coords)


removeBorders :: M.Map Coord (Tile Coord) -> S.Set Coord
removeBorders picture = S.fromList $ do
    (C y x, Tile _ tile) <- M.toList picture
    C y' x' <- tile
    guard $ all (between 1 8) [y', x']
    return $ C (y * 8 + y' - 1) (x * 8 + x' - 1)

snake :: [Coord]
snake =
  toPicture
    ["                  # "
    ,"#    ##    ##    ###"
    ," #  #  #  #  #  #   "]

countSnakes :: S.Set Coord -> Int
countSnakes image = waves . length $ do
    snakes <- reorient snake
    coord <- C <$> [0..8*12] <*> [0..8*12]
    guard $ all (\pixel -> add coord pixel `S.member` image) snakes
    return ()
        where waves n = length image - length snake * n


setBits :: [Int] -> Int
setBits = foldl' (\acc n -> acc .|. 1 `shift` n) 0

topIdx, leftIdx, bottomIdx, rightIdx :: [Coord] -> Int
topIdx     xs = setBits [  i | C 0 i <- xs]
leftIdx    xs = setBits [  i | C i 0 <- xs]
bottomIdx  xs = setBits [9-i | C 9 i <- xs]
rightIdx   xs = setBits [  i | C i 9 <- xs]
