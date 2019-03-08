{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}
module HKD.Indexed where

import           Data.Barbie
import           Data.Either.Validation
import           Data.Functor.Identity
import           Data.Monoid
import           HKD.Transformer
import           HKD.User
import           HKD.Validation
import           Language.Haskell.DoNotation
import           Control.Monad.Indexed.State
import           Control.Monad.Indexed
import           Prelude                     hiding (Monad(..), pure)

type HKDT b f g = IxState (b f) (b g)

validate'
  :: (IxMonadState m, ProductB b) => b Validator -> m (b Identity) (b (Validation [Error])) ()
validate' v = imodify (!> v)

transform' :: (IxMonadState m, ProductB b, Functor f) => b Endo -> m (b f) (b f) ()
transform' f = imodify (%> f)

actions :: IxState (UserB Identity) (UserB (Validation [Error])) ()
actions = do
  transform' normalizeUser
  validate' userValidations

pipe :: UserB Identity -> UserB (Validation [Error])
pipe = (%> normalizeUser) . (!> userValidations)
