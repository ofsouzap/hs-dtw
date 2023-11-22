module DtwTests where

import Test.HUnit
    ( assertEqual,
      runTestTT,
      Counts(failures),
      Test(TestList, TestCase, TestLabel), Assertion, AssertionPredicable (assertionPredicate), assertBool )
import System.Exit ( exitFailure, exitSuccess )
import GHC.Stack (HasCallStack)

import Dtw (dtwRecursiveSlow, CostVal, dtwMemoising)

type DtwFunc a x = (a -> a -> x) -> [a] -> [a] -> Maybe ([(Int, Int)], x)

-- Test case inputs

case1SeqA :: [Double]
case1SeqA = [1, 1, 1, 2, 3, 4]

case1SeqB :: [Double]
case1SeqB = [1, 2, 3, 4]

case1CostFunc :: Double -> Double -> Double
case1CostFunc x y = (x-y)^2

case2SeqA :: [Double]
case2SeqA = [1, 3, 5, 3, 3]

case2SeqB :: [Double]
case2SeqB = [5, 2, 2, 3, 5, 1]

case2CostFunc :: Double -> Double -> Double
case2CostFunc = max

-- Utility functions

assertEqualOneOf :: (HasCallStack, Eq a, Show a) => String -> [a] -> a -> Assertion
assertEqualOneOf msg exps out = assertBool msg (out `elem` exps)

getCasePart :: (Int, Int) -> ([a], [a]) -> ([a], [a])
getCasePart (x, y) (seqA, seqB) = (take (x+1) seqA, take (y+1) seqB)

genTest :: CostVal x => (a -> a -> x) -> [a] -> [a] -> DtwFunc a x -> (Int, Int) -> Maybe ([(Int, Int)], x)
genTest cost fullA fullB dtwFunc pos = let (seqA, seqB) = getCasePart pos (fullA, fullB) in dtwFunc cost seqA seqB

genCase1Test :: DtwFunc Double Double -> (Int, Int) -> Maybe ([(Int, Int)], Double)
genCase1Test = genTest case1CostFunc case1SeqA case1SeqB

genCase2Test :: DtwFunc Double Double -> (Int, Int) -> Maybe ([(Int, Int)], Double)
genCase2Test = genTest case2CostFunc case2SeqA case2SeqB

-- Tests

-- Case 1 Tests

-- testRecSlowCase1? :: Test
-- testRecSlowCase1? = TestLabel "RecSlow-Case1-?" (TestCase
--     (
--         assertEqual ""
--         (Just ([  ], ?))
--         (genRecSlowCase1Test (?, ?))
--     ))

genTestCase1Negative :: DtwFunc Double Double -> Test
genTestCase1Negative dtw = TestLabel "RecSlow-Case1-Negative" (TestCase
    (
        assertEqual ""
        Nothing
        (genCase1Test dtw (-1, -4))
    ))

genTestCase1OutOfBounds :: DtwFunc Double Double -> Test
genTestCase1OutOfBounds dtw = TestLabel "RecSlow-Case1-OutOfBounds" (TestCase
    (
        assertEqual ""
        Nothing
        (genCase1Test dtw (4, 5))
    ))

genTestCase1Full :: DtwFunc Double Double -> Test
genTestCase1Full dtw = TestLabel "RecSlow-Case1-Full" (TestCase
    (
        assertEqual ""
        (Just ([ (0,0), (1,0), (2,0), (3,1), (4,2), (5,3) ], 0))
        (genCase1Test dtw (5, 3))
    ))

genTestCase1At01 :: DtwFunc Double Double -> Test
genTestCase1At01 dtw = TestLabel "RecSlow-Case1-0,1" (TestCase
    (
        assertEqual ""
        (Just ([ (0,0), (0,1) ], 1))
        (genCase1Test dtw (0, 1))
    ))

genTestCase1At50 :: DtwFunc Double Double -> Test
genTestCase1At50 dtw = TestLabel "RecSlow-Case1-5,0" (TestCase
    (
        assertEqual ""
        (Just ([ (0,0), (1,0), (2,0), (3,0), (4,0), (5,0) ], 14))
        (genCase1Test dtw (5, 0))
    ))

genTestCase1At33 :: DtwFunc Double Double -> Test
genTestCase1At33 dtw = TestLabel "RecSlow-Case1-3,3" (TestCase
    (
        assertEqual ""
        (Just ([ (0,0), (1,0), (2,0), (3,1), (3,2), (3,3) ], 5))
        (genCase1Test dtw (3, 3))
    ))

genTestCase1At42 :: DtwFunc Double Double -> Test
genTestCase1At42 dtw = TestLabel "RecSlow-Case1-4,2" (TestCase
    (
        assertEqual ""
        (Just ([ (0,0), (1,0), (2,0), (3,1), (4,2) ], 0))
        (genCase1Test dtw (4, 2))
    ))

genTestCase1At41 :: DtwFunc Double Double -> Test
genTestCase1At41 dtw = TestLabel "RecSlow-Case1-4,1" (TestCase
    (
        assertEqual ""
        (Just ([ (0,0), (1,0), (2,0), (3,1), (4,1) ], 1))
        (genCase1Test dtw (4, 1))
    ))

-- Case 2 Tests

-- testRecSlowCase2? :: Test
-- testRecSlowCase2? = TestLabel "RecSlow-Case2-?" (TestCase
--     (
--         assertEqual ""
--         (Just ([  ], ?))
--         (genRecSlowCase2Test (?, ?))
--     ))

genTestCase2Negative :: DtwFunc Double Double -> Test
genTestCase2Negative dtw = TestLabel "RecSlow-Case2-Negative" (TestCase
    (
        assertEqual ""
        Nothing
        (genCase2Test dtw (-3, 1))
    ))

genTestCase2OutOfBounds :: DtwFunc Double Double -> Test
genTestCase2OutOfBounds dtw = TestLabel "RecSlow-Case2-OutOfBounds" (TestCase
    (
        assertEqual ""
        Nothing
        (genCase2Test dtw (1, 8))
    ))

genTestCase2Full :: DtwFunc Double Double -> Test
genTestCase2Full dtw = TestLabel "RecSlow-Case2-Full" (TestCase
    (
        assertEqual ""
        (Just 11)
        (fmap snd (genCase2Test dtw (4, 5))))
    )

genTestCase2At00 :: DtwFunc Double Double -> Test
genTestCase2At00 dtw = TestLabel "RecSlow-Case2-0,0" (TestCase
    (
        assertEqual ""
        (Just ([ (0,0) ], 0))
        (genCase2Test dtw (0, 0))
    ))

genTestCase2At14 :: DtwFunc Double Double -> Test
genTestCase2At14 dtw = TestLabel "RecSlow-Case2-1,4" (TestCase
    (
        assertEqual ""
        (Just ([ (0,0), (0,1), (0,2), (0,3), (1,4) ], 5))
        (genCase2Test dtw (1, 4))
    ))

genTestCase2At13 :: DtwFunc Double Double -> Test
genTestCase2At13 dtw = TestLabel "RecSlow-Case2-1,3" (TestCase
    (
        assertEqual ""
        (Just ([ (0,0), (0,1), (0,2), (2,3) ], 4))
        (genCase2Test dtw (1, 3))
    ))

genTestCase2At30 :: DtwFunc Double Double -> Test
genTestCase2At30 dtw = TestLabel "RecSlow-Case2-3,0" (TestCase
    (
        assertEqual ""
        (Just ([ (0,0), (1,0), (2,0), (3,0) ], 3))
        (genCase2Test dtw (3, 0))
    ))

genTestCase2At42 :: DtwFunc Double Double -> Test
genTestCase2At42 dtw = TestLabel "RecSlow-Case2-4,2" (TestCase
    (
        assertEqual ""
        (Just ([ (0,0), (1,0), (2,0), (3,1), (3,2) ], 8))
        (genCase2Test dtw (4, 2))
    ))

testGens :: [DtwFunc Double Double -> Test]
testGens =
    [
        genTestCase1Full,
        genTestCase1At01,
        genTestCase1At50,
        genTestCase1At33,
        genTestCase1At42,
        genTestCase1At41
    ]

testFuncs :: [DtwFunc Double Double]
testFuncs =
    [
        dtwRecursiveSlow,
        dtwMemoising
    ]

testPairs :: [(DtwFunc Double Double -> Test, DtwFunc Double Double)]
testPairs = [(x, y) | x <- testGens, y <- testFuncs]

tests :: Test
tests = TestList (map (\ (f, x) -> f x) testPairs)

main :: IO ()
main = do
    result <- runTestTT tests
    if failures result > 0 then exitFailure else exitSuccess
