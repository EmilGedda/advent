{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE BangPatterns #-}
module Advent.Solution.DayEleven where

import           Advent.Problem               (Day, day, count, Parseable, parseString, both)
import           Data.Bifunctor               (bimap)
import           Data.Foldable                (forM_)
import           Data.Maybe                   (mapMaybe, listToMaybe)
import           Data.Tuple                   (swap)
import           Data.Word                    (Word8)
import           Control.Arrow                ((&&&))
import           Control.Monad                (filterM, when, filterM)
import           Control.Monad.ST             (ST, runST)
import qualified Data.Vector.Mutable          as M
import qualified Data.Vector.Unboxed          as U
import qualified Data.Vector.Unboxed.Mutable  as UM

type Tile = Word8
data Ruleset = Simple | Extended deriving Enum

floor' :: Tile
floor' = 0

occupied' :: Tile
occupied' = 1

empty' :: Tile
empty' = 2

newtype Grid = Grid { fromGrid :: (Int, U.Vector Tile) }

toTile 'L' = empty'
toTile '.' = floor'

instance Parseable Grid where
    parseString = Grid . (length . takeWhile (/='\n') &&&
        U.fromList . map toTile . filter (/='\n'))

day11 :: Day
day11 = day 11 (seat Simple) (seat Extended)

seat :: Ruleset -> Grid -> Int
seat rules (Grid (width,v)) = runST $ stabilize rules width =<< U.thaw v

adjacent :: Ruleset -> Int -> UM.MVector s Tile -> [Int] -> ST s (M.MVector s (UM.MVector s Int))
adjacent rules width tiles seats = do
    let coords = swap . flip divMod width
        height = UM.length tiles`div` width
        index (x,y) = x + y * width
        neighbouring = [(1, 0), (-1, 1), (0, 1), (1, 1)]
        oob (x,y) = x < 0 || x >= width || y < 0 || y >= height
        chair c = (/= floor') <$> UM.unsafeRead tiles c
        directions = (\c -> map (\n -> both (n*) c) [1..]) <$> neighbouring

    neighbours <- M.new (UM.length tiles)
    lengths    <- UM.new (UM.length tiles)

    forM_ seats $ \seat -> do
        M.unsafeWrite neighbours seat =<< UM.new 8
        UM.unsafeWrite lengths seat 0

    forM_ seats $ \seat -> do
        let (x, y) = coords seat
            offset = bimap (+x) (+y)

        surrounding <-
            case rules of
               Simple -> filterM (fmap (/=floor') . UM.unsafeRead tiles)
                                 . map index . filter (not . oob)
                                 $ map offset neighbouring

               Extended -> mapMaybe listToMaybe
                           <$> mapM (filterM chair. map index
                                    . takeWhile (not . oob)
                                    . map offset) directions

        current <- M.unsafeRead neighbours seat
        forM_ surrounding $ \neighbour -> do
            !x <- UM.unsafeRead lengths seat
            UM.unsafeWrite current x neighbour
            UM.unsafeWrite lengths seat (x + 1)

            other <- M.unsafeRead neighbours neighbour
            !y <- UM.unsafeRead lengths neighbour
            UM.unsafeWrite other y seat
            UM.unsafeWrite lengths neighbour (y + 1)

    forM_ seats $ \seat -> do
        n <- UM.unsafeRead lengths seat
        v <- M.unsafeRead neighbours seat
        M.unsafeWrite neighbours seat (UM.slice 0 n v)

    return neighbours

tile :: Ruleset -> Int -> Tile -> Tile
tile rules occupied t
    | t == empty', occupied == 0 = occupied'
    | t == occupied', occupied > 3 + fromEnum rules = empty'
    | otherwise = t

stabilize :: Ruleset -> Int -> UM.MVector s Tile -> ST s Int
stabilize rules width v = do
    let !tolerance = 3 + fromEnum rules

    !loop <- filterM (fmap (/=floor') . UM.unsafeRead v) [0..UM.length v - 1]
    neighbours <- adjacent rules width v loop
    changes <- UM.new (length loop)

    tiles <- U.thaw $ U.fromList loop

    whileM $ do
        changed <-
            foldIM (UM.length tiles) 0 $ \idx changed -> do
                i <- UM.unsafeRead tiles idx
                current <- UM.unsafeRead v i
                close <- M.unsafeRead neighbours i

                occupied <- foldIM (UM.length close) 0 $ \j n -> do
                    if n > tolerance
                       then return n
                       else do
                        seat <- UM.unsafeRead close j
                        tile <- UM.unsafeRead v seat
                        return . (n+) . fromEnum $ tile == occupied'

                let new = tile rules occupied current
                if current /= new
                   then do
                       UM.unsafeWrite changes changed (i, new)
                       return $ changed + 1
                   else return changed

        forM_ [0..changed - 1] $ \i -> do
            (idx, tile) <- UM.unsafeRead changes i
            UM.unsafeWrite v idx tile

        return $ changed > 0

    taken v

taken :: UM.MVector s Tile -> ST s Int
taken v =  count (==occupied') <$> mapM (UM.unsafeRead v) [0..UM.length v - 1]

whileM :: Monad m => m Bool -> m ()
whileM f = do
    !b <- f
    when b (whileM f)

foldIM max s f = go 0 max s f
    where go curr max !s f
            | curr >= max = return s
            | otherwise = do
                !newS <- f curr s
                go (curr + 1) max newS f
