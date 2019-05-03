{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module HKD.Options where

import Options.Applicative hiding (Failure, Success)
import Data.Semigroup ((<>))
import Data.Functor.Identity
import Data.Functor.Compose
import Data.Barbie
import System.Environment
import GHC.Generics (Generic)
import Text.Read
import qualified Data.Aeson as A
import Data.Aeson.Lens
import Data.Text.Lens
import Control.Lens
import Data.Maybe
import Data.Foldable
import Data.Either.Validation

type Options = OptionsF Identity

data OptionsF f =
    OptionsF
    { serverHost :: f String
    , numThreads :: f Int
    , verbosity  :: f Int
    }
    deriving (Generic, FunctorB, TraversableB, ProductB, ConstraintsB, ProductBC)

deriving instance (AllBF Show f OptionsF) => Show (OptionsF f)
deriving instance (AllBF Eq f OptionsF) => Eq (OptionsF f)

instance (Alternative f) => Semigroup (OptionsF f) where
  (<>) = bzipWith (<|>)

instance (Alternative f) => Monoid (OptionsF f) where
  mempty = buniq empty

mkOptional :: FunctorB b => b Parser -> b (Parser `Compose` Maybe)
mkOptional = bmap (Compose . optional)

toParserInfo :: (TraversableB b) => b (Parser `Compose` Maybe) -> ParserInfo (b Maybe)
toParserInfo b = info (bsequence b) briefDesc

cliOpts :: OptionsF Parser
cliOpts =
    OptionsF
    { serverHost =
          strOption (long "hello" <> metavar "TARGET" <> help "Target for the greeting")
    , numThreads =
          option auto
                 (long "threads" <> short 't' <> help "number of threads" <> metavar "INT")
    , verbosity  = option auto
                          (long "verbosity"
                           <> short 'v'
                           <> help "Level of verbosity"
                           <> metavar "VERBOSITY")
    }

getCliOpts :: IO (OptionsF Maybe)
getCliOpts = execParser $ toParserInfo (mkOptional cliOpts)

--- Env Opts

lookupEnv' :: Read a => String -> (IO `Compose` Maybe) a
lookupEnv' envKey = Compose $ do
    lookupEnv envKey >>= pure . \case
        Just x -> readMaybe x
        Nothing -> Nothing

envOpts :: IO (OptionsF Maybe)
envOpts = bsequence
    OptionsF
    { serverHost = lookupEnv' "SERVER_HOST"
                <|> lookupEnv' "SERVER" -- allow several possible keys
    , numThreads = lookupEnv' "NUM_THREADS"
    , verbosity    = Compose (pure empty) -- Don't set verbosity from environment
    }

--- File Opts

jsonOpts :: A.Value -> OptionsF Maybe
jsonOpts = bsequence
    OptionsF
    { serverHost = Compose $ preview (key "host" . _String . unpacked)
    , numThreads = Compose $ preview (key "num_threads" . _Number . to round)
    , verbosity  = Compose $ preview (key "verbosity" . _Number . to round)
    }

jsonValue :: A.Value
-- jsonValue = A.object ["host" A..= A.String "example.com", "verbosity" A..= A.Number 42]
jsonValue = A.object []

orDefault :: ProductB b => b Maybe -> b Identity -> b Identity
orDefault = (bzipWith (flip fromMaybeI))
  where
    fromMaybeI :: Identity a -> Maybe a -> Identity a
    fromMaybeI ia ma = Identity $ fromMaybe (runIdentity ia) ma

optErrors :: OptionsF (Const String)
optErrors =
    OptionsF
    { serverHost = "server host not provided but is required"
    , numThreads = "num threads not provided"
    , verbosity  = "verbosity not provided"
    }

getErrs :: (TraversableB b, ProductB b)
        => b Maybe
        -> b (Const String)
        -> Validation [String] (b Identity)
getErrs mOpts errMsgs = bsequence' $ bzipWith go mOpts errMsgs
  where
    go :: Maybe a -> Const String x -> Validation [String] a
    go (Just x) _ = Success x
    go Nothing (Const err) = Failure [err]

defaultOpts :: OptionsF Identity
defaultOpts =
    OptionsF
    { serverHost = pure "localhost"
    , numThreads = pure 1
    , verbosity  = pure 0
    }

getOptions :: IO (Validation [String] Options)
getOptions = do
    mOpts <- fold [getCliOpts, envOpts, pure (jsonOpts jsonValue)]
    return $ getErrs mOpts optErrors
    -- return $ mOpts `orDefault` defaultOpts
