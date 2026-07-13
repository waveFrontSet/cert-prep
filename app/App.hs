{-# LANGUAGE LambdaCase #-}

module App where

import CLI (CLIOptions (..), cliConfigPath)
import Control.Monad.Except (ExceptT, MonadError (..), runExceptT)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadIO (liftIO), MonadReader, ReaderT (..), asks)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Explanations (ExplainConfig, mkExplainConfig)
import Registry (loadFile, loadRegistry)
import Sampling (SamplingStrategy (..), sampleQuestions)
import Settings qualified
import System.Environment (lookupEnv)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import System.Random (newStdGen)
import TUI.ConfigSelect (selectConfig)
import Types (Config (..), Question)

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
            registry <- liftIO loadRegistry
            if null registry
                then throwError NoConfigSelected
                else do
                    mPath <- liftIO $ selectConfig registry
                    maybe (throwError NoConfigSelected) pure mPath

loadConfig :: FilePath -> App Config
loadConfig p = do
    result <- liftIO $ loadFile p
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
    case NE.nonEmpty sampledQuestions of
        Nothing -> throwError NoQuestionsInConfig
        Just ne -> pure ne

loadSettings :: App Settings.Settings
loadSettings = do
    settings <- liftIO Settings.loadSettings
    either (throwError . SettingsParseError) return settings

resolveExplainConfig :: Settings.Settings -> App (Maybe ExplainConfig)
resolveExplainConfig settings = do
    apiKey <- liftIO $ lookupEnv "GEMINI_API_KEY"
    return $ mkExplainConfig settings apiKey
