-- (AI) Explanations for reasoning behind correct answers
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLists #-}

module Explanations where

import Data.Text (Text)
import Data.Text qualified as T
import OpenAI.V1
import OpenAI.V1.Chat.Completions
import OpenAI.V1.Models qualified as Models
import System.Environment (getEnv)
import System.Environment.Blank (getEnvDefault)

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
