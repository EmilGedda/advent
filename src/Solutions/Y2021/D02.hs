module Solutions.Y2021.D02 (day02) where

import Advent.Problem
import Data.Attoparsec.ByteString.Char8 hiding (count)

data Instruction = Instruction !String !Int deriving (Generic, NFData)

data Submarine = Submarine { position :: !Coord, aim :: !Int }

instance Parseable Instruction where
    parseInput = attoparse $ Instruction <$> many1 letter_ascii <*> (char ' ' *> decimal)


day02 :: Day 2
day02 = day (submarine simple) (submarine complex)


submarine :: (Submarine -> Instruction -> Submarine) -> [Instruction] -> Int
submarine part = combineCoord (*) . position . foldl' part (Submarine (Coord 0 0) 0)

simple :: Submarine -> Instruction -> Submarine
simple (Submarine pos aim) (Instruction instr d)
  = case instr of
      "forward" -> Submarine (pos + Coord 0 d) aim
      "down"    -> Submarine (pos + Coord d 0) aim
      "up"      -> Submarine (pos - Coord d 0) aim

complex :: Submarine -> Instruction -> Submarine
complex (Submarine pos aim) (Instruction instr d)
  = case instr of
      "forward" -> Submarine (pos + Coord (aim * d) d) aim
      "down"    -> Submarine pos (aim + d)
      "up"      -> Submarine pos (aim - d)
