module DtwMath
(
  Unbounded,
  min3By
)
where

data Unbounded =
    Val Int
  | PosInf
  | NegInf

-- |Find the minimum of 3 items comparing them by a function applied to them
min3By :: (Ord b) => (a -> b) -> a -> a -> a -> a
min3By f x y z = let yzMin = if f y < f z then y else z in
  if f x < f yzMin then x else yzMin
