{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Advent
import Advent.API
import Advent.Problem
import Solutions

import Test.Tasty.Bench

import Control.Monad.Except   (runExceptT, ExceptT(..), MonadError)
import Control.Monad.Reader   (ReaderT)
import Control.Monad.Catch    (MonadCatch)
import Data.ByteString hiding (putStrLn)

type App r a = ReaderT r (ExceptT String IO) a
type MonadApp m = (MonadHTTP m, MonadError String m, MonadCatch m, MonadFS m)

output :: (a -> IO (Either String b)) -> (b -> IO ()) -> a -> IO ()
output f g input = either putStrLn g =<< f input

(<==) :: (a -> IO ()) -> App NetworkEnv a -> IO ()
(<==) = output $ runExceptT . runNetworkEnv
infixr 0 <==

main :: IO ()
main = defaultMain <== mapM (\(WrapYear y) -> benchYear y) years

benchYear :: MonadApp m => Year _ _ -> m Benchmark
benchYear y@(Year solutions) = bgroup (('Y':) . show $ yearNum y)
                           <$> mapM day (toDayList solutions)
        where day (WrapDay d) = benchDay (yearNum y) d

benchDay :: MonadApp m => Integer -> Day _ -> m Benchmark
benchDay year d@(Day (partOne :: a -> b) partTwo) = do
    Input !input <- fetchInput year (dayNum d)
    let text = parseInput input
    return $ bgroup (('D':) . show $ dayNum d)
                    [ bench "Parsing" $ nf (parseInput :: ByteString -> a) input
                    , bench "Part one" $ nf partOne text
                    , bench "Part two" $ nf partTwo text
                    ]
