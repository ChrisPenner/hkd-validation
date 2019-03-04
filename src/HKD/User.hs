{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE DeriveAnyClass #-}
module HKD.User where

import Data.Barbie
import GHC.Generics
import Data.Functor.Identity

-- type User = UserB Bare Identity

data UserB f =
  UserB { userId    :: f String
        , country   :: f String
        , interests :: f [String]
        , age       :: f Int
        }
    deriving (Generic, FunctorB, TraversableB, ProductB, ConstraintsB, ProductBC)

deriving instance (forall a. Show a => Show (f a)) => Show (UserB f)


type family AutoStrip f a where
  AutoStrip Identity a = a
  AutoStrip f a = f a
