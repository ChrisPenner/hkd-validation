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

module HKD.Aeson where

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

data User f = User
  { userId :: f String
  , country :: f String
  , interests :: f [String]
  , age :: f Int
  } deriving ( Generic
             , FunctorB, TraversableB, ProductB, ConstraintsB, ProductBC
             )

deriving instance (forall a. Show a => Show (f a)) => Show (User f)

jsonKeys :: User (Const T.Text)
jsonKeys = User
  { userId    = Const "user_id"
  , country   = Const "country"
  , interests = Const "interests"
  , age       = Const "age"
  }


userFromJson :: User (ReaderT Value Maybe)
userFromJson = bmapC @FromJSON (atKey . getConst) jsonKeys
 where
  atKey :: FromJSON a => T.Text -> ReaderT Value Maybe a
  atKey k = do
    v <- ask
    lift (preview (key k) v >>= fromResult . fromJSON)

fromResult :: Result a -> Maybe a
fromResult (Success a) = Just a
fromResult _           = Nothing

bmapC :: forall c f g b. (AllB c b, ProductBC b) => (forall a. c a => f a -> g a) -> b f -> b g
bmapC f = bzipWith withDict bdicts
  where
    withDict :: forall a. Dict c a -> f a -> g a
    withDict d fa = requiringDict (f fa) d



jsonInstances :: User (Dict FromJSON)
jsonInstances = bdicts

