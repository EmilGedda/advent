{-# LANGUAGE TemplateHaskell #-}
module Solutions.TH where

import Advent.Problem
import Control.Monad
import Data.Char
import Data.List
import Data.Maybe
import Text.Printf
import Language.Haskell.TH

solutions :: Integer -> [String]
solutions year = map (\d -> printf "Solutions.Y%d.D%02d.day%02d" year d d) [1 :: Int ..25]

isSolution :: Name -> Q Bool
isSolution func = do
    VarI _ typ _ <- reify func
    return $ "AppT (ConT Advent.Problem.Day)" `isPrefixOf` show typ

discoverDays :: Q Exp
discoverDays = discoverDays' . read . dropWhile (not . isDigit) . loc_module =<< location

discoverDays' :: Integer -> Q Exp
discoverDays' y = do
    candidates <- mapM lookupValueName $ solutions y
    daylist <- filterM isSolution $ catMaybes candidates
    empty <- [|EmptyDays|]
    push <- [|PushDay|]
    let days = foldr (AppE . AppE push . VarE) empty daylist
    container <- [|Year|]
    return $ AppE container days
