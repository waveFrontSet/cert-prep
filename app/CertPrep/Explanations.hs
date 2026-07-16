-- (AI) Explanations for reasoning behind correct answers
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLists #-}

module CertPrep.Explanations (
    ExplainEnv (..),
    ExplainError (..),
    ExplainEvent (..),
    ExplainRequest (..),
    MonadExplain (..),
    mkExplainEnv,
    renderExplainPrompt,
    renderExplainError,
) where

import CertPrep.Settings (Settings (aiBaseUrl, aiModel, aiSystemPrompt))
import CertPrep.Types (AnswerResult, Question (..), userSelectedAnswers)
import Control.Exception (SomeException, try)
import Data.IntSet (toList)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import OpenAI.V1 (
    Methods (Methods, createChatCompletionStreamTyped),
    getClientEnv,
    makeMethods,
 )
import OpenAI.V1.Chat.Completions (
    CreateChatCompletion (messages, model),
    Message (System, User, content, name),
    _CreateChatCompletion,
 )
import OpenAI.V1.Chat.Completions qualified as Comp
import OpenAI.V1.Chat.Completions.Stream (
    ChatCompletionChunk (ChatCompletionChunk, choices),
    ChunkChoice (delta),
    Delta (delta_content),
 )
import OpenAI.V1.Models qualified as Models

newtype ExplainEnv = ExplainEnv
    { explainStream :: Text -> (ExplainEvent -> IO ()) -> IO ()
    }

data ExplainError
    = ExplainHttpError Text
    | ExplainEmptyResponse
    deriving (Show, Eq)

-- One streamed token batch, the clean end of the stream, or a failure.
data ExplainEvent
    = ExplainChunk Text
    | ExplainDone
    | ExplainFailed ExplainError
    deriving (Show, Eq)

data ExplainRequest = ExplainRequest
    { reqId :: Int -- distinguishes this request from abandoned earlier ones
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
    let Methods{createChatCompletionStreamTyped} =
            makeMethods clientEnv (T.pack apiKey) Nothing Nothing
    return $ Just $ ExplainEnv{explainStream = stream createChatCompletionStreamTyped}
  where
    -- Total: every outcome (including exceptions) becomes an ExplainEvent.
    stream createStream prompt emit = do
        result <- try @SomeException $
            createStream (chatRequest prompt) $ \case
                Left err -> emit (ExplainFailed (ExplainHttpError err))
                Right chunk ->
                    let t = chunkText chunk
                     in if T.null t then return () else emit (ExplainChunk t)
        emit $ case result of
            Left err -> ExplainFailed (ExplainHttpError (T.pack $ show err))
            Right () -> ExplainDone
    chatRequest prompt =
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
    chunkText ChatCompletionChunk{choices} =
        foldMap (fromMaybe "" . delta_content . delta) choices

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
