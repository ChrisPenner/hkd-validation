{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE QuantifiedConstraints #-}
module HKD.Environment where

import System.Environment
import Data.Barbie
import GHC.Generics
import Data.Functor.Const
import Data.Functor.Identity
import Text.Read

getEnvVars :: (AllB Read b, ConstraintsB b, TraversableB b) => b (Const String) -> IO (b Maybe)
getEnvVars = btraverseC @Read readEnvVar
  where
    readEnvVar (Const env) = readMaybe <$> getEnv env

data Config f =
  Config { hostname :: f String
         , port     :: f Int
         }
    deriving (Generic, FunctorB, TraversableB, ProductB, ConstraintsB, ProductBC)

deriving instance (forall a. Show a => Show (f a)) => Show (Config f)

configKeys :: Config (Const String)
configKeys = Config
  { hostname = "HOSTNAME"
  , port = "PORT"
  }

getConfig :: IO (Config Identity)
getConfig = bsequence' . bmapC @Read (fmap read . getEnv . getConst) $ configKeys
