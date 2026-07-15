-- (AI) Explanations for reasoning behind correct answers
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLists #-}

module Explanations (
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
import Data.IntSet (toList)
import Data.Text (Text)
import Data.Text qualified as T
import OpenAI.V1 (
    Methods (Methods, createChatCompletion),
    getClientEnv,
    makeMethods,
 )
import OpenAI.V1.Chat.Completions (
    ChatCompletionObject (ChatCompletionObject, choices),
    Choice (message),
    CreateChatCompletion (messages, model),
    Message (User, content, name),
    messageToContent,
    _CreateChatCompletion,
 )
import OpenAI.V1.Chat.Completions qualified as Comp
import OpenAI.V1.Models qualified as Models
import Settings (Settings (aiBaseUrl, aiModel, aiSystemPrompt))
import Types (AnswerResult, Question (..), userSelectedAnswers)

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
                            { content = [Comp.Text{text = explainSystemPrompt cfg}]
                            , name = Just "system"
                            }
                        , User
                            { content = [Comp.Text{text = prompt}]
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

renderExplainPrompt :: Question -> AnswerResult -> Text
renderExplainPrompt qu ar = renderQuestion qu <> renderAnswerResult ar
  where
    commaSeparated = T.intercalate ", " . fmap (T.pack . show) . toList
    renderQuestion q =
        "Question: "
            <> text q
            <> "\nAnswer Choices:\n"
            <> zippedAnswers (answerChoices q)
            <> "Correct Answers: "
            <> commaSeparated (correctAnswer q)
    zippedAnswers answers =
        T.unlines $ zipWith (\a answer -> T.pack (show @Int a) <> ". " <> answer) [0 ..] answers
    renderAnswerResult aResult = "\nUser Selected Answers: " <> commaSeparated (userSelectedAnswers aResult)

renderExplainError :: ExplainError -> Text
renderExplainError (ExplainHttpError t) = "Explanation failed: " <> t
renderExplainError ExplainEmptyResponse{} = "Explanation failed: empty response."
