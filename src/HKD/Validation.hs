{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuantifiedConstraints #-}

module HKD.Validation where

import           Control.Lens

import qualified Data.Aeson             as A
import           Data.Barbie
import           Data.Either
import           Data.Either.Validation
import qualified Data.Map               as M
import           Data.Bifunctor
import           HKD.Transformer
import           Control.Arrow hiding (first)

import           HKD.User

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

runValidator ::  a -> Validator a -> Validation [Error] a
runValidator  a (Validator f) =
  case f a of
    []   -> Success a
    errs -> Failure errs

newtype Validator a = Validator {runV :: a -> [String] }
    deriving newtype (Semigroup, Monoid)

type CrossValidator b f = Kleisli Validator (b f)

-- Allows higher order validations which depend on other fields
-- E.g. a state must match the country field
crossValidate :: forall b. (ProductB b, FunctorB b)
              => b (CrossValidator b Identity)
              -> b Identity
              -> b (Validation [Error])
crossValidate v b = validate b validators
  where
    validators :: b Validator
    validators = bmap (\(Kleisli f) -> f b) v

using :: (b Identity -> Identity x) -> (x -> Validator r) -> CrossValidator b Identity r
using getter v = Kleisli (v . runIdentity . getter)

isolated :: Validator a -> CrossValidator UserB Identity a
isolated = Kleisli . const

userValidations :: UserB Validator
userValidations =
  UserB { userId    = atLeastLength 3 <> lessThanEqLength 10
        , country   = inList countryCodes
        , state     = mempty
        , interests = lessThanEqLength 2
        , age       = greaterThan 0
        }

stateList :: String -> [String]
stateList "CA" = ["SK", "AB", "ON"]
stateList "US" = ["DC", "AZ", "CA", "AB"]
stateList _ = []


higherOrderValidations :: UserB (CrossValidator UserB Identity)
higherOrderValidations =
  UserB { userId    = isolated $ atLeastLength 3 <> lessThanEqLength 10
        , country   = isolated $ inList countryCodes
        , state     = using country   $ \c -> inList (stateList c)
        , interests = isolated $ lessThanEqLength 2
        , age       = isolated $ greaterThan 0
        }


validate :: (ProductB b) => b Identity -> b Validator -> b (Validation [Error])
validate = bzipWith (runValidator . runIdentity)

infixl 9 !>
(!>) :: (ProductB b) => b Identity -> b Validator -> b (Validation [Error])
(!>) = validate

validated :: UserB (Validation [Error])
validated = testUser !> userValidations %> normalizeUser

withFieldNames :: UserB (Validation [Error]) -> UserB (Validation [Error])
withFieldNames b = bzipWith addName userFieldNames b
  where
    addName :: Const String a -> Validation [Error] a -> Validation [Error] a
    addName (Const name) = first (fmap ((name <> ": ") <>))

testUser :: UserB Identity
testUser =
  UserB { userId    = pure "a"
        , country   = pure "CasdfA"
        , state     = pure "AB"
        , interests = pure ["dogs"]
        , age       = pure 32
        }

userDefaults :: UserB Maybe
userDefaults =
  UserB { userId    = Nothing
        , country   = pure "US"
        , state     = pure "AB"
        , interests = pure ["food"]
        , age       = Nothing
        }

jsonMaybe :: A.FromJSON a => A.Value -> Maybe a
jsonMaybe x = case A.fromJSON x of
  A.Success a -> Just a
  A.Error _   -> Nothing

userFieldNames :: UserB (Const String)
userFieldNames =
  UserB { userId    = "user_id"
        , country   = "country"
        , state     = "state"
        , interests = "interests"
        , age       = "age"
        }


userFromMap :: M.Map String A.Value -> UserB Maybe
userFromMap m = bmapC @A.FromJSON lookupVal userFieldNames
  where
    lookupVal :: A.FromJSON a => Const String a -> Maybe a
    lookupVal (Const k) = M.lookup k m >>= jsonMaybe

printErrors :: UserB (Either [Error]) -> [Error]
printErrors = bfoldMap (fromLeft [])
