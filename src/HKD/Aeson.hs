{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE QuantifiedConstraints #-}

module HKD.Aeson where

import           Control.Applicative
import           Control.Lens
import           Control.Monad.Reader

import HKD.User
import           Data.Aeson
import           Data.Aeson.Lens
import           Data.Barbie
import           Data.ByteString.Lazy

import qualified Data.Text               as T


jsonKeys :: UserB (Const T.Text)
jsonKeys =
  UserB { userId    = Const "user_id"
        , country   = Const "country"
        , state     = Const "state"
        , interests = Const "interests"
        , age       = Const "age"
        }

userFromJson :: (AllB FromJSON b, TraversableB b, ConstraintsB b) => b (Const T.Text)-> ByteString -> (b Maybe)
userFromJson jsonKeys' = btraverseC @FromJSON (decoder . getConst) jsonKeys'
  where
    decoder :: FromJSON a => T.Text -> ByteString -> (Maybe a)
    decoder k =
       decode @Value >=> preview (key k) >=> fromResult . fromJSON
    fromResult :: Result a -> Maybe a
    fromResult = preview folded
