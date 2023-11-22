module Dtw
  (
  CostVal,
  dtwRecursiveSlow
  )
where

import Data.Maybe (Maybe)
import Data.Array (Array, array, (!))
import Data.Bifunctor (bimap)
import DtwMath (Unbounded, min3By)
import Data.Foldable (Foldable(fold))

class Ord x => CostVal x where
  zero :: x
  (.+.) :: x -> x -> x

instance CostVal Double where
  zero = 0.0
  (.+.) = (+)

instance CostVal Int where
  zero = 0
  (.+.) = (+)

-- |Calculate a minimal sequence of steps to link two sequences according and the total cost of the linking.
-- |Returns a list of pairs of indexes in the input lists.
-- |This implementation is performed recursively and doesn't memoise its results and so is very inefficient
dtwRecursiveSlow :: CostVal x => (a -> a -> x) -> [a] -> [a] -> Maybe ([(Int, Int)], x)
dtwRecursiveSlow _ [] _ = Nothing  -- Can't perform on empty list
dtwRecursiveSlow _ _ [] = Nothing  -- Can't perform on empty list
dtwRecursiveSlow cost xs ys = finaliseOutput (optimumAt (n-1, m-1)) where
  n = length xs
  m = length ys

  xsArr = array (0, n) (zip [0..] xs)
  ysArr = array (0, m) (zip [0..] ys)

  -- |Get the cost using indexes into the input lists as parameters
  costIdxs x y = cost (xsArr!x) (ysArr!y)

  -- |Perform final operations on the output to make it ready for returning
  finaliseOutput Nothing = Nothing
  finaliseOutput (Just (pairs, c)) = Just (
    reverse pairs,
    c
    )

  optimumAt (0, 0) = Just ([(0, 0)], zero)
  optimumAt (x, y) = case findMinPrev (prevsFor (x, y)) of
    Nothing -> Nothing
    Just (prevSeq, prevCost) -> Just ((x, y) : prevSeq, costIdxs x y .+. prevCost)
    where

    -- |Find the possible states that could be used as previous states for the given state
    prevsFor :: (Int, Int) -> [(Int, Int)]
    prevsFor (0, 0) = []
    prevsFor (x, 0) = [(x-1, 0)]
    prevsFor (0, y) = [(0, y-1)]
    prevsFor (x, y)
      | 0 <= x && x < n && 0 <= y && y < m = [(x-1, y-1), (x-1, y), (x, y-1)]
      | otherwise = []

    -- |Find the optimal previous state given a list of possible previous states
    -- findMinPrev :: [(Int, Int)] -> Maybe ([(Int, Int)], x)  -- Had to comment this out because I can't get scoped parameters to work
    findMinPrev = foldl foldFunc Nothing where
      foldFunc prev new@(x, y) = case optimumAt (x, y) of
        Nothing -> prev
        Just (newSeq, newCost) -> case prev of
          Nothing -> Just (newSeq, newCost)
          Just (_, pc) -> if newCost < pc then Just (newSeq, newCost) else prev
