{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module HKD.Options where

import System.Environment (lookupEnv, setEnv)
import Text.Read (readMaybe)
import Data.Functor.Compose (Compose(..))
import GHC.Generics (Generic)
import qualified Data.Aeson as A
import Data.Barbie
import Data.Text.Lens
import Data.Aeson.Lens
import Control.Lens
import Data.Either.Validation
import Data.Foldable
import Options.Applicative hiding (Failure, Success)

type Options = Options_ Identity

data Options_ f =
    Options_
    { serverHost :: f String
    , numThreads :: f Int
    , verbosity  :: f Int
    }
    deriving (Generic, FunctorB, TraversableB, ProductB, ConstraintsB, ProductBC)

deriving instance (AllBF Show f Options_) => Show (Options_ f)
deriving instance (AllBF Eq f Options_) => Eq (Options_ f)
deriving instance (AllBF A.FromJSON f Options_) => A.FromJSON (Options_ f)

instance (Alternative f) => Semigroup (Options_ f) where
  (<>) = bzipWith (<|>)

instance (Alternative f) => Monoid (Options_ f) where
  mempty = buniq empty

mkOptional :: FunctorB b => b Parser -> b (Parser `Compose` Maybe)
mkOptional = bmap (Compose . optional)

toParserInfo :: (TraversableB b) => b (Parser `Compose` Maybe) -> ParserInfo (b Maybe)
toParserInfo b = info (bsequence b) briefDesc

cliOptsParser :: Options_ Parser
cliOptsParser =
    Options_
    { serverHost =
          strOption (long "serverHost" <> metavar "HOST" <> help "host for API interactions")
    , numThreads =
          option auto
                 (long "threads" <> short 't' <> help "number of threads" <> metavar "INT")
    , verbosity  = option auto
                          (long "verbosity"
                           <> short 'v'
                           <> help "Level of verbosity"
                           <> metavar "VERBOSITY")
    }

cliOpts :: IO (Options_ Maybe)
cliOpts = execParser $ toParserInfo (mkOptional cliOptsParser)

--- Env Opts

readEnv :: Read a => String -> (IO `Compose` Maybe) a
readEnv envKey = Compose $ do
    lookupEnv envKey >>= pure . \case
        Just x -> readMaybe x
        Nothing -> Nothing

envOpts :: IO (Options_ Maybe)
envOpts = bsequence
    Options_
    { serverHost = Compose . lookupEnv $ "SERVER_HOST"
    , numThreads = readEnv "NUM_THREADS"
    , verbosity    = Compose . pure $ Nothing -- Don't read verbosity from environment
    }

--- File Opts


jsonOptsDerived :: (A.FromJSON (b Maybe), ProductB b) => A.Value -> b Maybe
jsonOptsDerived = fromResult . A.fromJSON
  where
    fromResult :: ProductB b => A.Result (b Maybe) -> b Maybe
    fromResult (A.Success a) = a
    fromResult (A.Error _) = buniq Nothing

jsonOptsCustom :: A.Value -> Options_ Maybe
jsonOptsCustom = bsequence
    Options_
    { serverHost = findField $ key "host"        . _String . unpacked
    , numThreads = findField $ key "num_threads" . _Number . to round
    , verbosity  = findField $ key "verbosity"   . _Number . to round
    }
      where
        findField :: Fold A.Value a -> Compose ((->) A.Value) Maybe a
        findField p = Compose (preview p)


readConfigFile :: IO A.Value
readConfigFile =
    pure $ A.object [ "host" A..= A.String "example.com"
                    , "verbosity" A..= A.Number 42
                    ]

-- readConfigFile = pure $ A.object []

withDefaults :: ProductB b => b Identity -> b Maybe -> b Identity
withDefaults = bzipWith fromMaybeI
  where
    fromMaybeI :: Identity a -> Maybe a -> Identity a
    fromMaybeI (Identity a) Nothing  = Identity a
    fromMaybeI _            (Just a) = Identity a

optErrors :: Options_ (Const String)
optErrors =
    Options_
    { serverHost = "server host required but not provided"
    , numThreads = "num threads required but not provided"
    , verbosity  = "verbosity required but not provided"
    }

validateOptions :: (TraversableB b, ProductB b)
                => b (Const String)
                -> b Maybe
                -> Validation [String] (b Identity)
validateOptions errMsgs mOpts = bsequence' $ bzipWith validate mOpts errMsgs
  where
    validate :: Maybe a -> Const String a -> Validation [String] a
    validate (Just x) _ = Success x
    validate Nothing (Const err) = Failure [err]

defaultOpts :: Options_ Identity
defaultOpts =
    Options_
    { serverHost = pure "localhost"
    , numThreads = pure 1
    , verbosity  = pure 0
    }

-- getOptionsBad :: IO Options
-- getOptionsBad = do
--     configJson <- readConfigFile
--     mServerHostEnv <- readServerHostEnv
--     mNumThreadsEnv <- readNumThreadsEnv
--     let mServerHostJson = configJson ^? key "server_host" . _String . unpacked
--     let mNumThreadsJson = configJson ^? key "num_threads" . _Number . to round
--     return $ Options <$> fromMaybe serverHostDef (mServerHostEnv <|> mServerHostJson)
--                      <*> fromMaybe numThreadsDef (mNumThreadsEnv <|> mNumThreadsJson)


getOptions :: IO (Validation [String] Options)
getOptions =
  validateOptions optErrors <$> fold [cliOpts, envOpts, jsonOptsCustom <$> readConfigFile]

-- getOptions :: IO (Validation [String] (Options_ Identity))
-- getOptions = do
--     setEnv "NUM_THREADS" "1337"
--     configJson <- readConfigFile
--     -- withDefaults defaultOpts <$> fold [envOpts, pure (jsonOptsCustom configJson)]
--     validateOptions optErrors <$> fold [envOpts, pure (jsonOptsCustom configJson)]
