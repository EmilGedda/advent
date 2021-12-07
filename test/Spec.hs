{-# LANGUAGE TupleSections #-}
import Types
import Tests

import Prelude              hiding  (null)
import Advent.Problem       hiding  (day)
import Advent.API                   (runNetworkEnv)
import Solutions                    (years)
import Data.ByteString              (null)
import Data.Either                  (isRight)
import Control.Monad.Except         (runExceptT, forM)
import Data.Maybe                   (catMaybes)
import Data.List                    ((\\), find)
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main =
    defaultMain
        . testGroup "Test solutions"
        =<< forM answers
             (\(Answers y ans) ->
                let Just alldays = (\(WrapYear (Year ds)) -> toDayList ds)
                             <$> find (\(WrapYear y') -> yearNum y' == y) years
                in testGroup (show y)
                   . (testConsistency alldays ans:)
                   . map (uncurry testDay)
                   <$> mapM (findTest y) ans)

testConsistency :: [SomeDay] -> [Answer] -> TestTree
testConsistency ds ans
    = testCase "No days lacking tests"
    $ map someDayNum ds \\ map (\(Answer d _ _) -> dayNum d) ans @?= []

findTest :: Integer -> Answer -> IO (Answer, Either String Input)
findTest y ans@(Answer d@(Day _ _) _ _)
    = (ans,) <$> (runExceptT . runNetworkEnv) (fetchInput y $ dayNum d)

testDay :: Answer -> Either String Input -> TestTree
testDay (Answer d@(Day partOne partTwo) first second) input =
    let wrap f = solution . f . parseInput . fromInput
        parts = zip3 "12" [wrap partOne, wrap partTwo] $ catMaybes [Just first, second]
        fromRight ~(Right r) = r
        fromLeft ~(Left a) = a
        day = 'D':show (dayNum d)
        test (part, output, answer) = after AllSucceed (day ++ ".Input") .
            testCase ("Part " ++ [part]) $ output (fromRight input) @?= solution answer
    in testGroup day $
            testCase "Input" (do
                isRight input @? "Input is invalid: " ++ fromLeft input
                (not . null . fromInput $ fromRight input) @? "Input is empty"
        ):map test parts

