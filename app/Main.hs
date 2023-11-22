module Main where

import Text.Show ()
import Dtw (dtwRecursiveSlow)

seqA :: [Int]
seqA = [1, 1, 1, 2, 3, 4]

seqB :: [Int]
seqB = [1, 2, 3, 4]

costFunc :: Int -> Int -> Int
costFunc x y = (x-y)^2

listToString :: Show a => [a] -> String
listToString [] = ""
listToString (h:ts) = show h ++ "," ++ listToString ts

runFor :: (Int, Int) -> Maybe ([(Int, Int)], Int)
runFor (x, y) = dtwRecursiveSlow costFunc (take (x+1) seqA) (take (y+1) seqB)

outputFor :: Maybe ([(Int, Int)], Int) -> String
outputFor x = case x of
    Nothing -> "Nothing found"
    Just (seq, cost) -> listToString seq ++ " [cost=" ++ show cost ++ "]"

doOutput :: (Int, Int) -> IO ()
doOutput (x, y) = do
    putStr ("(" ++ show x ++ ", " ++ show y ++ ") => ")
    (putStrLn . outputFor . runFor) (x, y)

main :: IO ()
main = do
    doOutput (0, 0)
    doOutput (1, 2)
    doOutput (3, 1)
    doOutput (5, 3)
