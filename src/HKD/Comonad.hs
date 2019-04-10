{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module HKD.Comonad where

import           Control.Comonad
import           Control.Lens
import           Data.Barbie
import           HKD.User
import           Control.Arrow
import           Data.Either.Validation
import           Data.Kind
import           Data.Generics.Product
import           GHC.TypeLits
import           GHC.Generics


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

dependantValidate :: (FunctorB b)
                  => b Identity
                  -> b (Kleisli (Validation String) (b Identity))
                  -> b (Validation String)
dependantValidate b v = bmap (\(Kleisli f) -> f b) v


type family AllC (c :: k -> Constraint) (xs :: [k]) :: Constraint where
  AllC _ '[] = ()
  AllC c (x:xs) = (c x, AllC c xs)

class WanderingComonad w (t :: [Symbol]) | w -> t where
    focusField :: forall (s :: Symbol) a x. HasField' s (w x) a => w x -> w a


data MyType = MyType { str :: String
                     , int :: Int
                     }

newtype FieldFocus r (s :: Symbol) = FF {runFocus :: r}
    deriving Generic

extractField :: forall s r a. HasField' s r a => FieldFocus r s -> a
extractField (FF r) = getField @s r

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


