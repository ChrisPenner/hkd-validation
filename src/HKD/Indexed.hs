{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}
module HKD.Indexed where

import           Control.Arrow

import           Data.Barbie

import           Data.Either.Validation
import           Data.Functor.Identity
import           Data.Monoid

import           HKD.Transformer
import           HKD.User

import           HKD.Validation

import           Language.Haskell.DoNotation

import           Control.Monad.Indexed.State

import           Prelude               hiding (Monad(..), pure)

type HKDT b f g = IxState (b f) (b g)

validate' :: forall b.
          ProductB b
          => b Validator
          -> HKDT b Identity (Validation [Error]) ()
validate' v = imodify (!> v)

transform' :: forall b f.
  (ProductB b, Functor f)
          => b Endo
          -> HKDT b f f ()
transform' f = imodify (%> f)

actions :: HKDT UserB Identity (Validation [Error]) ()
actions = do
  transform' normalizeUser
  validate' userValidations
