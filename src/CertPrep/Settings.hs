module CertPrep.Settings where

import CertPrep.Common (configDir, loadFileWithDefault)
import Data.Aeson (
    FromJSON (parseJSON),
    withObject,
    (.!=),
    (.:?),
 )
import System.FilePath ((</>))

data Settings = Settings
    { aiModel :: Text
    , aiBaseUrl :: Text
    , aiSystemPrompt :: Text
    }
    deriving (Show, Eq, Generic)

instance FromJSON Settings where
    parseJSON = withObject "Settings" $ \v ->
        Settings
            <$> v .:? "aiModel" .!= aiModel defaultSettings
            <*> v .:? "aiBaseUrl" .!= aiBaseUrl defaultSettings
            <*> v .:? "aiSystemPrompt" .!= aiSystemPrompt defaultSettings

defaultSettings :: Settings
defaultSettings =
    Settings
        { aiModel = "gemini-2.5-flash"
        , aiBaseUrl = "https://generativelanguage.googleapis.com/v1beta/openai"
        , aiSystemPrompt = defaultPrompt
        }
  where
    defaultPrompt =
        "You are a friendly, concise and to-the-point mentor for Cloud certification questions.\n\
        \ You are given a (possibly multi-select) question with correct answers \
        \ and the given answer by the user. Your task is to explain the correct answer \
        \ and the reasoning behind it. Additionally, explain why the incorrect answers \
        \ (in particular the user's choices) are incorrect.\
        \ At the end of your explanations, always list relevant links to the documentation."

settingsFilePath :: (MonadIO m) => m FilePath
settingsFilePath = (</> "settings.json") <$> configDir

loadSettings :: (MonadIO m) => m (Either String Settings)
loadSettings = loadFileWithDefault (Just defaultSettings) =<< settingsFilePath
