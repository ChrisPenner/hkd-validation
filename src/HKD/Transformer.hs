module HKD.Transformer where

import HKD.User
import Data.Char
import Data.Monoid
import Data.Barbie

strip :: Endo String
strip = Endo $ takeWhile (not . isSpace) . dropWhile isSpace

upperCase :: Endo String
upperCase = Endo $ fmap toUpper


normalizeUser :: UserB Endo
normalizeUser =
  UserB { userId    = upperCase
        , country   = strip
        , interests = mempty
        , age       = mempty
        }


transform :: (ProductB b, Functor f) => b f -> b Endo -> b f
transform = bzipWith runTransform
  where
    runTransform fa (Endo f) = f <$> fa

infixl 9 %>
(%>) :: (ProductB b, Functor f) => b f -> b Endo -> b f
(%>) = transform
