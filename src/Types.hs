module Types (
    Question (..),
    Answer,
    isCorrect,
    AnswerResult (..),
    evalAnswer,
    Config (..),
) where

import Data.Aeson (
    FromJSON (parseJSON),
    Options (fieldLabelModifier),
    ToJSON (toJSON),
    defaultOptions,
    genericParseJSON,
    genericToJSON,
 )
import Data.Char (toLower)
import Data.IntSet (IntSet, difference, intersection)
import Data.List (stripPrefix)
import Data.Map.Strict (Map)
import Data.Maybe (fromMaybe)
import Data.Text (Text)

import GHC.Generics (Generic)

type Answer = IntSet

data Question = Question
    { questionText :: Text
    , questionAnswerChoices :: [Text]
    , questionCorrectAnswer :: Answer
    , questionCategory :: Maybe Text
    }
    deriving (Show, Eq, Generic)

data AnswerResult = AnswerResult
    { answerResultCorrect :: IntSet
    , answerResultMissing :: IntSet
    , answerResultWrong :: IntSet
    }
    deriving (Show, Eq)

evalAnswer :: Question -> Answer -> AnswerResult
evalAnswer q ans =
    AnswerResult
        { answerResultCorrect = questionCorrectAnswer q `intersection` ans
        , answerResultMissing = questionCorrectAnswer q `difference` ans
        , answerResultWrong = ans `difference` questionCorrectAnswer q
        }

isCorrect :: Question -> Answer -> Bool
isCorrect q ans = questionCorrectAnswer q == ans

data Config = Config
    { configQuestions :: [Question]
    , configSampleAmount :: Int
    , configCategoryWeights :: Maybe (Map Text Int)
    }
    deriving (Show, Eq, Generic)

toLowerFirstLetter :: String -> String
toLowerFirstLetter [] = []
toLowerFirstLetter (x : xs) = toLower x : xs

prefixStripOptions :: String -> Options
prefixStripOptions prefix =
    defaultOptions
        { fieldLabelModifier = \s ->
            toLowerFirstLetter $ fromMaybe s (stripPrefix prefix s)
        }

instance FromJSON Question where
    parseJSON = genericParseJSON (prefixStripOptions "question")
instance ToJSON Question where
    toJSON = genericToJSON (prefixStripOptions "question")

instance FromJSON Config where
    parseJSON = genericParseJSON (prefixStripOptions "config")
instance ToJSON Config where
    toJSON = genericToJSON (prefixStripOptions "config")
