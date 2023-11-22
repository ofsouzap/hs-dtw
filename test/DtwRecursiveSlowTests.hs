module DtwRecursiveSlowTests where

import Test.HUnit
    ( assertEqual,
      runTestTT,
      Counts(failures),
      Test(TestList, TestCase) )
import System.Exit ( exitFailure, exitSuccess )

import Dtw (dtwRecursiveSlow)

-- Test case inputs

case1SeqA :: [Double]
case1SeqA = [1, 1, 1, 2, 3, 4]

case1SeqB :: [Double]
case1SeqB = [1, 2, 3, 4]

case1costFunc :: Double -> Double -> Double
case1costFunc x y = (x-y)^2

-- Utility functions

getCasePart :: (Int, Int) -> ([(a, a)], [(a, a)]) -> ([(a, a)], [(a, a)])
getCasePart (x, y) (seqA, seqB) = (take (x+1) seqA, take (y+1) seqB)

-- Tests

testCase1Full :: Test
testCase1Full = TestCase
    (
        assertEqual ""
        (Just ([ (0,0), (1,0), (2,0), (3,1), (4,2), (5,3) ], 0))
        (dtwRecursiveSlow case1costFunc case1SeqA case1SeqB)
    )

tests :: Test
tests = TestList [testCase1Full]

main :: IO ()
main = do
    result <- runTestTT tests
    if failures result > 0 then exitFailure else exitSuccess
