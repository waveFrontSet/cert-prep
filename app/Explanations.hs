-- (AI) Explanations for reasoning behind correct answers
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLists #-}

module Explanations (
    explain,
    ExplainConfig (..),
    ExplainError (..),
    ExplainRequest (..),
    MonadExplain (..),
    fetchExplanation,
    mkExplainConfig,
    renderExplainPrompt,
    renderExplainError,
) where

import Control.Exception (SomeException, try)
import Data.Text (Text)
import Data.Text qualified as T
import OpenAI.V1
import OpenAI.V1.Chat.Completions
import OpenAI.V1.Models qualified as Models
import Settings
import System.Environment (getEnv)
import System.Environment.Blank (getEnvDefault)
import Types (AnswerResult, Question)

data ExplainConfig = ExplainConfig
    {explainApiKey, explainModel, explainBaseUrl, explainSystemPrompt :: Text}

data ExplainError
    = ExplainHttpError Text
    | ExplainEmptyResponse
    deriving (Show, Eq)

data ExplainRequest = ExplainRequest
    { reqQuestionIndex :: Int
    , reqPrompt :: Text
    }
    deriving (Show, Eq)

-- Fire-and-forget: returning () (not the result) is what lets the TuiM
-- instance be async and a test instance be a recorder.
class (Monad m) => MonadExplain m where
    requestExplanation :: ExplainRequest -> m ()
    explainAvailable :: m Bool

mkExplainConfig :: Settings -> Maybe String -> Maybe ExplainConfig
mkExplainConfig _ Nothing = Nothing
mkExplainConfig s (Just apiKey) =
    Just
        ExplainConfig
            { explainApiKey = T.pack apiKey
            , explainModel = aiModel s
            , explainBaseUrl = aiBaseUrl s
            , explainSystemPrompt = aiSystemPrompt s
            }

fetchExplanation :: ExplainConfig -> Text -> IO (Either ExplainError Text)
fetchExplanation cfg prompt = do
    choices <- try @SomeException $ do
        clientEnv <- getClientEnv (explainBaseUrl cfg)
        let Methods{createChatCompletion} =
                makeMethods clientEnv (explainApiKey cfg) Nothing Nothing
        ChatCompletionObject{choices} <-
            createChatCompletion
                _CreateChatCompletion
                    { messages =
                        [ User
                            { content = [Text{text = explainSystemPrompt cfg}]
                            , name = Just "system"
                            }
                        , User
                            { content = [Text{text = prompt}]
                            , name = Nothing
                            }
                        ]
                    , model = Models.Model (explainModel cfg)
                    }
        return choices
    return $
        case choices of
            Left err -> Left (ExplainHttpError (T.pack $ show err))
            Right [] -> Left ExplainEmptyResponse
            Right cs -> Right $ foldr ((<>) . messageToContent . message) (T.pack "") cs

renderExplainPrompt :: Question -> AnswerResult -> Text -- replaces show-based prompt
renderExplainPrompt q aResult = T.pack $ show q <> show aResult

renderExplainError :: ExplainError -> Text
renderExplainError (ExplainHttpError t) = "Explanation failed: " <> t
renderExplainError ExplainEmptyResponse{} = "Explanation failed: empty response."

data GeminiConfig = GeminiConfig
    { geminiApiKey :: Text
    , geminiBaseUrl :: Text
    {- ^ e.g. "https://generativelanguage.googleapis.com/v1beta/openai"
    or   "https://REGION-aiplatform.googleapis.com/v1beta1/projects/PROJECT_ID/locations/REGION/endpoints/openapi"
    -}
    , geminiModel :: Text
    -- ^ e.g. "gemini-2.5-flash"
    , systemPrompt :: Text
    }

defaultPrompt :: String
defaultPrompt =
    "You are an expert for Cloud certification questions.\n\
    \ You are given a (possibly multi-select) question with correct answers \
    \ and the given answer by the user. Your task is to explain the correct answer \
    \ and the reasoning behind it. Additionally, explain why the incorrect answers \
    \ (in particular the user's choices) are incorrect."

geminiConfigFromEnv :: IO GeminiConfig
geminiConfigFromEnv = do
    key <- T.pack <$> getEnv "GEMINI_API_KEY"
    baseUrl <- T.pack <$> getEnv "GEMINI_BASE_URL"
    geminiModel <- T.pack <$> getEnv "GEMINI_MODEL"
    systemPrompt <- T.pack <$> getEnvDefault "GEMINI_PROMPT" defaultPrompt
    return
        GeminiConfig
            { geminiApiKey = key
            , geminiBaseUrl = baseUrl
            , geminiModel = geminiModel
            , systemPrompt = systemPrompt
            }

explain :: Text -> IO Text
explain prompt = do
    cfg <- geminiConfigFromEnv
    clientEnv <- getClientEnv (geminiBaseUrl cfg)
    let Methods{createChatCompletion} =
            makeMethods clientEnv (geminiApiKey cfg) Nothing Nothing
    ChatCompletionObject{choices} <-
        createChatCompletion
            _CreateChatCompletion
                { messages =
                    [ User
                        { content = [Text{text = systemPrompt cfg}]
                        , name = Just "system"
                        }
                    , User
                        { content = [Text{text = prompt}]
                        , name = Nothing
                        }
                    ]
                , model = Models.Model (geminiModel cfg)
                }
    return $ foldr ((<>) . messageToContent . message) (T.pack "") choices
