{-# LANGUAGE TemplateHaskell #-}
module Solutions.TH where

import Advent.Problem
import Control.Monad
import Data.Char
import Data.Maybe
import Text.Printf
import Language.Haskell.TH

solutions :: Integer -> [String]
solutions year = map (\d -> printf "Solutions.Y%d.D%02d.day%02d" year d d) [1 :: Int ..25]

isSolution :: Name -> Q Bool
isSolution func = do
    VarI _ typ _ <- reify func
    return $ show typ == "ConT Advent.Problem.Day"

discoverDays :: Q Exp
discoverDays = do
    loc <- location
    let year = read . dropWhile (not . isDigit) $ loc_module loc
    candidates <- mapM lookupValueName $ solutions year
    solutions <- filterM isSolution $ catMaybes candidates
    container <- runQ [|Year year|]
    [| $(pure $ AppE container . ListE $ map VarE solutions) |]
