module Main where

import Text.Show
import Dtw (dtwRecursiveSlow)

seqA :: [Double]
seqA = [1, 1, 1, 2, 3, 4]

seqB :: [Double]
seqB = [1, 2, 3, 4]

costFunc :: Double -> Double -> Double
costFunc x y = (x-y)^2

listToString :: Show a => [a] -> String
listToString [] = ""
listToString (h:ts) = show h ++ "," ++ listToString ts

main :: IO ()
main = do
    putStrLn (case dtwRecursiveSlow costFunc seqA seqB of
        Nothing -> "Nothing found"
        Just (seq, cost) -> listToString seq ++ " (" ++ show cost ++ ")")
