module Solutions.Y2020.D21 (day21) where

import           Advent.Problem
import           Control.Arrow          (second)
import           Data.Char              (isPunctuation)
import           Data.List              (sortOn, foldl1', intersect, intercalate)
import qualified Data.HashSet           as S
import qualified Data.HashMap.Strict    as M

data Food = Food [String] [String]

instance Parseable Food where
    parseString = uncurry Food
                . second (drop 1)
                . break (=="contains")
                . words
                . filter (not . isPunctuation)

day21 :: Day
day21 = day 21 (length . safe) (intercalate "," . map snd . sortOn fst . allergens)

safe :: [Food] -> [String]
safe foods = intersect ingredients
           . S.toList
           . S.difference (S.fromList ingredients)
           . S.fromList
           . map snd
           $ allergens foods
    where ingredients = concatMap (\(Food i _) -> i) foods

allergens :: [Food] -> [(String, String)]
allergens = go
          . sortOn (S.size . snd)
          . M.toList
          . foldl1' (M.unionWith S.intersection)
          . map possible
    where possible (Food i a) = M.fromList $ zip a (repeat $ S.fromList i)
          go [] = []
          go ((a, i):xs) = (a, head $ S.toList i)
                         : (go . sortOn (S.size . snd)
                         $ map (second $ flip S.difference i) xs)
