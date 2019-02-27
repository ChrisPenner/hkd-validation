{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuantifiedConstraints #-}

module HKD.Validation where

import           Control.Lens

import           Data.Aeson
import           Data.Barbie
import           Data.Barbie.Constraints
import           Data.Either
import qualified Data.Map                as M

import           GHC.Generics            (Generic)

type User = UserB Identity

data UserB f =
  UserB { userId    :: f String
        , country   :: f String
        , interests :: f [String]
        , age       :: f Int
        }
    deriving (Generic, FunctorB, TraversableB, ProductB, ConstraintsB, ProductBC)

deriving instance (forall a. Show a => Show (f a)) => Show (UserB f)
type Error = String

-- type Validator a = a -> [Error]
-- newtype Val a = Val (Kleisli (Either [Error]) a a)
atLeastLength :: (Foldable f, Show (f a)) => Int -> Validator (f a)
atLeastLength n =
  predToValidation (\e -> show e <> " must be at least size " <> show n)
  $ (>= n) . length

lessThanEqLength :: (Foldable f, Show (f a)) => Int -> Validator (f a)
lessThanEqLength n =
  predToValidation (\e -> show e <> " must be no longer than " <> show n)
  $ (<= n) . length

predToValidation :: (a -> String) -> (a -> Bool) -> Validator a
predToValidation err f =
  Validator
  $ \a -> if f a
      then []
      else [err a]

greaterThan :: (Show n, Ord n) => n -> Validator n
greaterThan n =
  predToValidation (\e -> show e <> " must be greater than " <> show n) (> n)

inList :: (Show e, Eq e) => [e] -> Validator e
inList xs =
  predToValidation (\e -> show e <> "not found in" <> show xs) (`elem` xs)

countryCodes :: [String]
countryCodes = ["CA", "US", "DE"]

runValidator :: Validator a -> a -> Either [String] a
runValidator (Validator f) a =
  case f a of
    []   -> Right a
    errs -> Left errs

newtype Validator a = Validator (a -> [String])
    deriving newtype (Semigroup, Monoid)

validations :: UserB Validator
validations =
  UserB { userId    = atLeastLength 3 <> lessThanEqLength 10
        , country   = inList countryCodes
        , interests = lessThanEqLength 2
        , age       = greaterThan 0
        }

runValidations :: UserB Validator -> User -> UserB (Either [Error])
runValidations = bzipWith runValidator'
  where
    runValidator' v (Identity a) = runValidator v a

validated :: UserB (Either [Error])
validated = runValidations validations testUser

bmapC :: forall c f g b.
      (AllB c b, ProductBC b)
      => (forall a. c a => f a -> g a)
      -> b f
      -> b g
bmapC f = bzipWith withDict bdicts
  where
    withDict :: forall a. Dict c a -> f a -> g a
    withDict d fa = requiringDict (f fa) d

testUser :: User
testUser =
  UserB { userId    = pure "a"
        , country   = pure "CasdfA"
        , interests = pure ["dogs"]
        , age       = pure 32
        }

userDefaults :: UserB Maybe
userDefaults =
  UserB { userId    = Nothing
        , country   = pure "US"
        , interests = pure ["food"]
        , age       = Nothing
        }

jsonMaybe :: FromJSON a => Value -> Maybe a
jsonMaybe x = case fromJSON x of
  Success a -> Just a
  Error _   -> Nothing

userFieldNames :: UserB (Const String)
userFieldNames =
  UserB { userId    = "user_id"
        , country   = "country"
        , interests = "interests"
        , age       = "age"
        }

userFromMap :: M.Map String Value -> UserB Maybe
userFromMap m = bmapC @FromJSON lookupVal userFieldNames
  where
    lookupVal :: FromJSON a => Const String a -> Maybe a
    lookupVal (Const k) = M.lookup k m >>= jsonMaybe

printErrors :: UserB (Either [Error]) -> [Error]
printErrors = bfoldMap (fromLeft [])
