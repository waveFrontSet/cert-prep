-- (AI) Explanations for reasoning behind correct answers
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLists #-}

module Explanations (
    ExplainEnv (..),
    ExplainError (..),
    ExplainRequest (..),
    MonadExplain (..),
    mkExplainEnv,
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
    Message (System, User, content, name),
    messageToContent,
    _CreateChatCompletion,
 )
import OpenAI.V1.Chat.Completions qualified as Comp
import OpenAI.V1.Models qualified as Models
import Settings (Settings (aiBaseUrl, aiModel, aiSystemPrompt))
import Types (AnswerResult, Question (..), userSelectedAnswers)

newtype ExplainEnv = ExplainEnv
    { explainFetch :: Text -> IO (Either ExplainError Text)
    }

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

mkExplainEnv :: Settings -> Maybe String -> IO (Maybe ExplainEnv)
mkExplainEnv _ Nothing = return Nothing
mkExplainEnv _ (Just "") = return Nothing
mkExplainEnv s (Just apiKey) = do
    clientEnv <- getClientEnv (aiBaseUrl s)
    let Methods{createChatCompletion} =
            makeMethods clientEnv (T.pack apiKey) Nothing Nothing
    return $ Just $ ExplainEnv{explainFetch = fetch createChatCompletion}
  where
    fetch ::
        (CreateChatCompletion -> IO ChatCompletionObject) ->
        Text ->
        IO (Either ExplainError Text)
    fetch createComp prompt = do
        choices <- try @SomeException $ do
            ChatCompletionObject{choices} <-
                createComp
                    _CreateChatCompletion
                        { messages =
                            [ System
                                { content = [Comp.Text{text = aiSystemPrompt s}]
                                , name = Just "system"
                                }
                            , User
                                { content = [Comp.Text{text = prompt}]
                                , name = Nothing
                                }
                            ]
                        , model = Models.Model (aiModel s)
                        }
            return choices
        return $
            case choices of
                Left err -> Left (ExplainHttpError (T.pack $ show err))
                Right [] -> Left ExplainEmptyResponse
                Right cs -> Right $ foldMap (messageToContent . message) cs

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
