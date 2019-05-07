{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE DeriveAnyClass #-}
module HKD.User where

import Data.Barbie
import GHC.Generics
import Data.Functor.Identity

type User = UserB Identity

data UserB f =
  UserB { userId    :: f String
        , country   :: f String
        , state     :: f String
        , interests :: f [String]
        , age       :: f Int
        }
    deriving (Generic, FunctorB, TraversableB, ProductB, ConstraintsB, ProductBC)

deriving instance (forall a. Show a => Show (f a)) => Show (UserB f)

simpleUser :: User
simpleUser =
    UserB
    { userId    = pure "USER-1234"
    , country   = pure "CA"
    , state     = pure "AB"
    , interests = pure ["dogs"]
    , age       = pure 32
    }

