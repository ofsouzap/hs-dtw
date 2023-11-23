module Main where

import Text.Show ()
import System.CPUTime ( getCPUTime )
import GHC.Arr ((!), listArray)
import Dtw (dtwRecursiveSlow, dtwMemoising)

type DtwFunc a x = (a -> a -> x) -> [a] -> [a] -> Maybe ([(Int, Int)], x)

n :: Int
n = 10

m :: Int
m = 10

seqA :: [Int]
seqA = [i `mod` 6 | i <- [0..n]]

seqB :: [Int]
seqB = [(i+5) `mod` 25 | i <- [0..m]]

costFunc :: Int -> Int -> Int
costFunc x y = (x-y)^2

listToString :: Show a => [a] -> String
listToString [] = ""
listToString (h:ts) = show h ++ "," ++ listToString ts

runFor :: DtwFunc Int Int -> (Int, Int) -> Maybe ([(Int, Int)], Int)
runFor dtw (x, y) = dtw costFunc (take (x+1) seqA) (take (y+1) seqB)

outputFor :: Maybe ([(Int, Int)], Int) -> String
outputFor x = case x of
    Nothing -> "Nothing found"
    Just (seq, cost) -> listToString seq ++ " [cost=" ++ show cost ++ "]"

doOutput :: DtwFunc Int Int -> (Int, Int) -> IO ()
doOutput dtw (x, y) = do
    putStr ("(" ++ show x ++ ", " ++ show y ++ ") => ")
    (putStrLn . outputFor . runFor dtw) (x, y)

-- |Just a very basic implementation that will time an IO computation and return it's execution time in seconds
timeIt :: IO () -> IO Double
timeIt io = do
    startTime <- getCPUTime
    io
    endTime <- getCPUTime
    let dPico = endTime - startTime in
        return (fromIntegral dPico * 1e-12)

runTestAsIO :: DtwFunc Int Int -> IO ()
runTestAsIO dtw = do
    let val = runFor dtw (n-1,m-1) in
        putStrLn (case val of
            Nothing -> "(Nothing found)"
            Just (_, val) -> "Value: " ++ show val)
    return ()

main :: IO ()
main = do
    putStrLn "Starting"
    slowTime <- timeIt (runTestAsIO dtwRecursiveSlow)
    memoisedTime <- timeIt (runTestAsIO dtwMemoising)
    putStrLn ("Slow time: " ++ show slowTime)
    putStrLn ("Memosied time: " ++ show memoisedTime)
