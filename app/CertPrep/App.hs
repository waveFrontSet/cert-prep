{-# LANGUAGE LambdaCase #-}

module CertPrep.App (
    AppEnv (..),
    Config (..),
    loadConfig,
    loadEarnedTrophies,
    loadSettings,
    registerConfig,
    resolveConfigPath,
    resolveExplainEnv,
    runApp',
    sampleNonEmpty,
)
where

import CertPrep.CLI (CLIOptions (..), cliConfigPath)
import CertPrep.Common (loadFile)
import CertPrep.Explanations (ExplainEnv, mkExplainEnv)
import CertPrep.Registry (loadRegistry, registerConfig)
import CertPrep.Sampling (SamplingStrategy (..), sampleQuestions)
import CertPrep.Settings qualified as Settings
import CertPrep.TUI (selectConfig)
import CertPrep.Trophy (loadEarnedTrophies)
import CertPrep.Types (Config (..), Question)
import Control.Monad.Error.Class (MonadError (..))
import Data.Map qualified as Map
import System.IO (hPutStrLn)
import System.Random (newStdGen)

newtype App a = App {unApp :: ReaderT AppEnv (ExceptT AppError IO) a}
    deriving
        ( Functor
        , Applicative
        , Monad
        , MonadReader AppEnv
        , MonadError AppError
        , MonadIO
        )

newtype AppEnv = AppEnv {cliOpts :: CLIOptions}

data AppError
    = NoConfigSelected
    | ConfigParseError String
    | NoQuestionsInConfig
    | SettingsParseError String
    deriving (Show)

renderError :: AppError -> String
renderError NoConfigSelected = "No config selected."
renderError (ConfigParseError msg) = "Config parse error: " ++ msg
renderError NoQuestionsInConfig = "No questions in config."
renderError (SettingsParseError msg) = "Settings parse error: " ++ msg

runApp' :: AppEnv -> App a -> IO a
runApp' env m =
    runExceptT (runReaderT (unApp m) env) >>= \case
        Left e -> hPutStrLn stderr (renderError e) >> exitFailure
        Right a -> pure a

resolveConfigPath :: App FilePath
resolveConfigPath = do
    opts <- asks cliOpts
    case cliConfigPath opts of
        Just p -> pure p
        Nothing -> do
            registry <- loadRegistry
            if null registry
                then throwError NoConfigSelected
                else do
                    mPath <- selectConfig registry
                    maybe (throwError NoConfigSelected) pure mPath

loadConfig :: FilePath -> App Config
loadConfig p = do
    result <- loadFile p
    either (throwError . ConfigParseError) return result

sampleNonEmpty :: Config -> App (NonEmpty Question)
sampleNonEmpty config = do
    opts <- asks cliOpts
    let sampleSize = fromMaybe (sampleAmount config) (cliSampleAmount opts)
        strategy = case cliWeights opts of
            [] -> maybe Uniform Stratified $ categoryWeights config
            ws -> Stratified (Map.fromList ws)
        allQuestions = questions config
        effectiveSize = min sampleSize (length allQuestions)
    gen <- liftIO newStdGen
    let sampledQuestions =
            sampleQuestions gen effectiveSize strategy allQuestions
    case nonEmpty sampledQuestions of
        Nothing -> throwError NoQuestionsInConfig
        Just ne -> pure ne

loadSettings :: App Settings.Settings
loadSettings = do
    settings <- Settings.loadSettings
    either (throwError . SettingsParseError) return settings

resolveExplainEnv :: Settings.Settings -> App (Maybe ExplainEnv)
resolveExplainEnv settings = do
    apiKey <- liftIO $ lookupEnv "GEMINI_API_KEY"
    mkExplainEnv settings apiKey
