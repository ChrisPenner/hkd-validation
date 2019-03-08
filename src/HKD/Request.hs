{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
module HKD.Request where

import Prelude hiding (Ordering(..))
import Network.Wai
import Control.Monad.Reader
import Data.Barbie
import Data.Functor.Compose
import qualified Data.Text as T
import Network.HTTP.Types.URI
import Text.Read
import GHC.Generics
import Data.Char

data SortBy = Created | Name

sortByFromString :: String -> Maybe SortBy
sortByFromString s = 
    case toLower <$> s of
      "created" -> Just Created
      "name"    -> Just Name
      _         -> Nothing

data Relation =
    LT
  | LTE
  | GT
  | GTE
  | EQ

data Filters f =
  Filters { city :: f String
          , age  :: f (Relation, Int)
          }
    deriving (Generic, FunctorB, TraversableB, ConstraintsB, ProductB)

data UserQuery f =
  UserQuery { limit   :: f Int
            , filters :: Filters f
            , sortBy  :: f SortBy
            }
    deriving (Generic, FunctorB, TraversableB, ConstraintsB, ProductB)


getQuery :: Read a => T.Text -> Request -> Maybe a
getQuery k r =
  let qs = queryToQueryText . queryString $ r
      mVal = join . lookup k $ qs
      result = mVal >>= readMaybe . T.unpack
   in result

filterFromRequest :: Filters ((->) Request `Compose` Maybe)
filterFromRequest = undefined


userQueryFromRequest :: Request -> UserQuery Maybe
userQueryFromRequest = bsequence $
  UserQuery { limit   = Compose $ getQuery "l"
            , filters = filterFromRequest
            , sortBy  = Compose $ sortByFromString <=< getQuery "sortBy"
            }
