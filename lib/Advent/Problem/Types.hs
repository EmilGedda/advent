{-# LANGUAGE TypeSynonymInstances #-}
module Advent.Problem.Types where

import           Control.DeepSeq        (NFData)
import           Data.Maybe             (fromJust)
import           Data.List.Split        (splitOn)
import           GHC.Generics           (Generic)
import           Data.Int               (Int32)
import qualified Data.Sequence          as S
import qualified Data.ByteString.Char8  as B
import qualified Data.Vector            as V
import qualified Data.Vector.Unboxed    as U

newtype CommaList a = CommaList { getList :: [a] } deriving (Generic, NFData)

class NFData a => Parseable a where
    parseInput :: B.ByteString -> a
    parseInput = parseString . B.unpack

    parseString :: String -> a
    parseString = parseInput . B.pack

instance Parseable Double where
    parseString = read

instance Parseable B.ByteString where
    parseInput = id

instance {-# OVERLAPS #-} Parseable String where
    parseString = id

instance Parseable Integer where
    parseInput = fst . fromJust . B.readInteger

instance Parseable Int where
    parseInput = fst . fromJust . B.readInt

instance Parseable Int32 where
    parseInput = fromIntegral . fst . fromJust . B.readInt

instance Parseable a => Parseable [a] where
    parseInput = map parseInput . B.lines

instance (Parseable a, U.Unbox a) => Parseable (U.Vector a) where
    parseInput = U.fromList . parseInput

instance Parseable a => Parseable (V.Vector a) where
    parseInput = V.fromList . parseInput

instance Parseable a => Parseable (CommaList a) where
    parseInput = CommaList . map parseInput . B.split ','

instance (Parseable a, Parseable b) => Parseable (a, b) where
    parseString i = let (a:b:_) = splitOn "\n\n" i in (parseString a, parseString b)

instance Parseable a => Parseable (S.Seq a) where
    parseInput = S.fromList . parseInput

instance Show a => Show (CommaList a) where
    show = show . getList

class NFData a => ToString a where
    solution :: a -> String

instance ToString Char where
    solution = return

instance ToString String where
    solution = id

instance ToString Int where
    solution = show

instance ToString Integer where
    solution = show

instance ToString Int32 where
    solution = show
