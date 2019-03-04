module HKD.VPro where

import Data.Profunctor
import Data.Profunctor.Composition
import Data.Either.Validation
import Control.Category

type VPro = Star (Validation [String])


-- data VPro a b = VPro (a -> b)


greaterThan :: (Show n, Ord n) => n -> VPro n n
greaterThan n = Star go
  where
    go m
      | m <= n = Failure [show m <> "must be greater than " <> show n]
      | otherwise = Success m
  
lessThan :: (Show n, Ord n) => n -> VPro n n
lessThan n = Star go
  where
    go m
      | m >= n = Failure [show m <> "must be less than " <> show n]
      | otherwise = Success m


-- both :: (Show n, Ord n, Num n) => VPro n n
-- both = procomposed $ lessThan 10 `Procompose` greaterThan 0
