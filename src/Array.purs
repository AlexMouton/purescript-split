module Data.Array.Split where

import Prelude

import Data.Array ((:), take, drop, filter, length)
import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\))

chop :: forall a b. (Array a -> Tuple b (Array a)) -> Array a -> Array b
chop _ [] = []
chop f as = b : chop f as'
  where
    (b /\ as') = f as

divvy :: forall a. Int -> Int -> Array a -> Array (Array a)
divvy _ _ [] = []
divvy n m as = filter (\ws -> (n == length ws)) choppedl
  where
    choppedl = chop (\xs -> take n xs /\ drop m xs) as
