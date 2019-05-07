{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}
module HKD.Comonad where

import           Control.Comonad
import           Control.Comonad.Store
import           Control.Lens
import           Data.Barbie
import           HKD.User
import           Control.Arrow
import           Data.Either.Validation
import           Data.Kind
import           Data.Generics.Product
import           GHC.TypeLits
import           GHC.Generics
import           Data.Type.Equality


bextract :: (FunctorB b, Comonad w) => b w -> b Identity
bextract = bmap (Identity . extract)

bextend :: (FunctorB b, Comonad w) => (forall a. w a -> a) -> b w -> b w
bextend f = bmap (extend f)


type Dependant b f = b (Kleisli f (b f))
bFix :: (FunctorB b) => Dependant b f -> b f
bFix b = let result = bmap (\(Kleisli f) -> f result) b in result

userFix :: Dependant UserB Identity
userFix =
    UserB { userId    = Kleisli $ country
          , country   = Kleisli $ Identity . const "CA"
          , state     = Kleisli $ Identity . const "AB"
          , interests = Kleisli $ Identity . const []
          , age       = Kleisli $ Identity . const 37
          }

userStore :: Store User String
userStore = store (extract . getField @"userId") simpleUser

selectField :: forall field s b w a. (HasField' field s b, ComonadStore s w) => w a -> b
selectField = getField @field . pos

selectType :: forall s b w a. (HasType b s, ComonadStore s w) => w a -> b
selectType = getTyped @b . pos

selectAcc :: ComonadStore s w => (s -> b) -> w a -> b
selectAcc f = f . pos

-- extendField :: forall s r a. HasField' s r a => (forall s' a'. HasField' s' r a' => FieldFocus r s' -> a') -> FieldFocus r s -> FieldFocus r s
-- extendField f (FF r) = undefined

-- class FieldCo r (xs :: [Symbol]) (ts :: [Type]) where
--     extendFields :: forall s a.
--                  (forall s' a'. HasField' s' r a' => FieldFocus r s' -> a')
--                 -> FieldFocus r s
--                 -> FieldFocus r s

-- instance FieldCo r '[] '[] where
--     -- | No transformations left; we can just return
--     extendFields _ = id

-- instance (HasField' s r t) => FieldCo r (s:xs) (t:ts) where
--   extendFields :: forall s a.
--                (forall s' a'. HasField' s' r a' => FieldFocus r s' -> a')
--                -> FieldFocus r s
--                -> FieldFocus r s
--   extendFields f d = setter (f' (coerce d :: FieldFocus r s) :: t) d
--     where
--       f' :: FieldFocus r s -> t
--       f' = f
--       setter :: t -> FieldFocus r s -> FieldFocus r s
--       setter a  = field @s .~ a


