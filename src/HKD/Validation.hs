{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
module HKD.Validation where

import Control.Lens
import Control.Monad.Reader

import Data.Aeson
import Data.Aeson.Lens
import Data.Barbie
import Data.Barbie.Constraints
import qualified Data.Text as T
import Data.Text.Lens
import Control.Applicative
import GHC.Generics (Generic)
import Control.Arrow

data User f = User
  { userId :: f String
  , country :: f String
  , interests :: f [String]
  , age :: f Int
  } deriving ( Generic
             , FunctorB, TraversableB, ProductB, ConstraintsB, ProductBC
             )

deriving instance (forall a. Show a => Show (f a)) => Show (User f)

type Error = String

newtype Inv p a = Inv (p a a)

type Validation a = Inv (Kleisli (Either [Error])) a

atLeastLength :: (Foldable f, Show (f a)) => Int -> Validation (f a)
atLeastLength n = predToValidation (\e -> show e <> " must be at least size " <> show n) $ (>=n) . length

lessThanEqLength :: (Foldable f, Show (f a)) => Int -> Validation (f a)
lessThanEqLength n = predToValidation (\e -> show e <> " must be no longer than " <> show n) $ (<=n) . length


predToValidation :: (a -> String) -> (a -> Bool) -> Validation a
predToValidation err f = Inv . Kleisli $ \a ->
  if f a
      then pure a
      else Left [err a]

greaterThan :: (Show n, Ord n) => n -> Validation n
greaterThan n = predToValidation (\e -> show e <> " must be greater than " <> show n) (>n)


inList :: (Show e, Eq e) => [e] -> Validation e
inList xs = predToValidation (\e -> show e <> "not found in" <> show xs) (`elem` xs)

countryCodes :: [String]
countryCodes = ["CA", "US", "DE"]

validations :: User (Inv (Kleisli (Either [Error])))
validations = User
  { userId = atLeastLength 3
  , country = inList countryCodes
  , interests = lessThanEqLength 2
  , age = greaterThan 0
  }

runValidations :: User (Inv (Kleisli (Either [Error])))
               -> User Identity
               -> User (Either [Error])
runValidations = bzipWith runValidation
  where
    runValidation (Inv (Kleisli f)) (Identity a) = f a

bmapC :: forall c f g b. (AllB c b, ProductBC b) => (forall a. c a => f a -> g a) -> b f -> b g
bmapC f = bzipWith withDict bdicts
  where
    withDict :: forall a. Dict c a -> f a -> g a
    withDict d fa = requiringDict (f fa) d

testUser :: User Identity
testUser = User
  { userId = pure "a"
  , country = pure "CA"
  , interests = pure ["dogs"]
  , age = pure 32
  }
