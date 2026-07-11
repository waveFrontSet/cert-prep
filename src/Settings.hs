module Settings where

import Data.Aeson (
    FromJSON,
    eitherDecodeStrict,
    withObject,
    (.!=),
    (.:?),
 )
import Data.Aeson.Types (FromJSON (parseJSON))
import Data.ByteString qualified as BS
import Data.Text (Text)
import GHC.Generics (Generic)
import System.Directory (XdgDirectory (XdgConfig), doesFileExist, getXdgDirectory)
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
        "You are an expert for Cloud certification questions.\n\
        \ You are given a (possibly multi-select) question with correct answers \
        \ and the given answer by the user. Your task is to explain the correct answer \
        \ and the reasoning behind it. Additionally, explain why the incorrect answers \
        \ (in particular the user's choices) are incorrect."

settingsFilePath :: IO FilePath
settingsFilePath = (</> "settings.json") <$> getXdgDirectory XdgConfig "cert-prep"

loadSettings :: IO (Either String Settings)
loadSettings = do
    path <- settingsFilePath
    exists <- doesFileExist path
    if not exists
        then return $ Right defaultSettings
        else eitherDecodeStrict <$> BS.readFile path
