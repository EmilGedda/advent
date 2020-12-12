{-# LANGUAGE ExistentialQuantification #-}
module Advent.Solution.DayEleven where

import           Advent.Problem               (Day, day, count, Parseable, parseString, both)
import           Data.Bifunctor               (bimap)
import           Data.Foldable                (foldrM, forM_)
import           Data.Maybe                   (mapMaybe, listToMaybe)
import           Data.Tuple                   (swap)
import           Control.Arrow                ((&&&))
import           Control.Monad                (filterM, when)
import           Control.Monad.Fix            (fix)
import           Control.Monad.ST             (ST, runST)
import qualified Data.Vector                  as V
import qualified Data.Vector.Mutable          as M

data Tile = Floor | Occupied | Empty deriving Eq
data Ruleset = Simple | Extended deriving Enum

newtype Grid = Grid { fromGrid :: (Int, V.Vector Tile) }

toTile 'L' = Empty
toTile '.' = Floor

instance Parseable Grid where
    parseString = Grid . (length . takeWhile (/='\n') &&&
        V.fromList . map toTile . filter (/='\n'))

day11 :: Day
day11 = day 11 (seat Simple) (seat Extended)

seat :: Ruleset -> Grid -> Int
seat rules (Grid (width,v)) = runST $ stabilize rules width =<< V.thaw v

-- precalculate this instead of doing it on every iteration
adjacent rules width v idx =
    let coords = swap . flip divMod width
        height = M.length v `div` width
        index (x,y) = x + y * width
        (x, y) = coords idx
        offset = bimap (x +) (y +)
        neighbouring = filter ((0,0) /=) [(a, b) | a <- [-1..1], b <- [-1..1]]
        oob (x,y) = x < 0 || x >= width || y < 0 || y >= height

     in case rules of
       Simple -> return . map index . filter ((x,y) /=)
                 . filter (not . oob) $ map offset neighbouring
       Extended ->
               let chair c = (/= Floor) <$> M.read v c
                   directions = (\c -> map (\n -> both (n*) c) [1..]) <$> neighbouring
               in mapMaybe listToMaybe
                  <$> mapM (filterM chair. map index
                            . takeWhile (not . oob) . map offset) directions

tile :: Ruleset -> [Tile] -> Tile -> Tile
tile rules neighbours t
    | t == Empty, Occupied `notElem` neighbours = Occupied
    | t == Occupied, count (==Occupied) neighbours > tolerance = Empty
    | otherwise = t
    where tolerance = 3 + fromEnum rules

stabilize :: Ruleset -> Int -> M.MVector s Tile -> ST s Int
stabilize rules width v = do
    let loop = [0..M.length v - 1]
        fold v f = foldrM f [] v

    -- whileM
    changes <-
        fold loop $ \i changed -> do
           current    <- M.read v i
           if current == Floor
              then return changed
              else do
                  neighbours <- mapM (M.read v) =<< adjacent rules width v i
                  let new = tile rules neighbours current
                  if current /= new
                     then return $ (i, new):changed
                     else return changed

    forM_ changes $
        uncurry (M.write  v)

    if not $ null changes
        then stabilize rules width v
        else taken v

taken :: M.MVector s Tile -> ST s Int
taken v =  count (==Occupied) <$> mapM (M.read v) [0..M.length v - 1]

whileM :: Monad m => m Bool -> m ()
whileM = fix ((=<<) . flip when =<<)


