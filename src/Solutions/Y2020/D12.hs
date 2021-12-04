{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
module Solutions.Y2020.D12 (day12) where

import Advent.Problem                   hiding (both)
import Lens.Micro
import Lens.Micro.Extras
import Lens.Micro.TH
import Data.Attoparsec.ByteString.Char8

newtype Action = Action { getAction :: (Char, Int) } deriving (Generic, NFData)

data Ship = Ship {
            _position :: (Int, Int),
            _direction :: (Int, Int),
            _waypoint :: (Int, Int)
          } deriving Show

makeLenses ''Ship

instance Parseable Action where
    parseInput = fromRight . parseOnly (curry Action <$> letter_ascii <*> decimal)

day12 :: Day 12
day12 = day (distance . sail position direction ship)
               (distance . sail waypoint waypoint ship)
    where ship = Ship (0,0) (0,1) (1,10)

sail :: Lens' Ship (Int, Int) ->  Lens' Ship (Int, Int) ->  Ship -> [Action] -> Ship
sail field target = foldl' . flip $ uncurry (act field target) . getAction

distance :: Ship -> Int
distance = uncurry (+) . (both %~ abs) . _position

act :: Lens' Ship (Int, Int) -> Lens' Ship (Int, Int) -> Char -> Int -> Ship -> Ship
act field  _ 'N' n = move field    =<< const (n, 0)
act field  _ 'S' n = move field    =<< const (-n,0)
act field  _ 'E' n = move field    =<< const (0, n)
act field  _ 'W' n = move field    =<< const (0,-n)
act _ target 'F' n = move position =<< towards target n
act _ target 'R' n = turn target   =<< rotate target (radians n)
act f target 'L' n = act f target 'R' (-n)

radians :: (Integral a, Floating b) => a -> b
radians n = fromIntegral n * pi / 180

rotate :: Lens' Ship (Int, Int) -> Double -> Ship -> (Int, Int)
rotate field n = (both %~ round) . angle n . (both %~ fromIntegral) . view field
    where angle n (x,y) = (cos n * x - sin n * y, sin n * x + cos n * y)

turn field  = update field const
move field  = update field (+)
towards f n = (both %~ (*n)) . view f
update l f  = (l %~) . tuple f
tuple f (a,b) (x,y) = (f a x, f b y)
