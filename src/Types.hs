module Types (
    Question (..),
    Answer,
    Category,
    isCorrect,
    AnswerResult (..),
    evalAnswer,
    Config (..),
) where

import Data.Aeson (FromJSON, ToJSON)
import Data.IntSet (IntSet, difference, intersection)
import Data.Map.Strict (Map)
import Data.Text (Text)

import GHC.Generics (Generic)

type Answer = IntSet
type Category = Text

data Question = Question
    { text :: Text
    , answerChoices :: [Text]
    , correctAnswer :: Answer
    , category :: Maybe Category
    }
    deriving (Show, Eq, Generic)

data AnswerResult = AnswerResult
    { correct :: IntSet
    , missing :: IntSet
    , wrong :: IntSet
    }
    deriving (Show, Eq)

evalAnswer :: Question -> Answer -> AnswerResult
evalAnswer q ans =
    AnswerResult
        { correct = correctAnswer q `intersection` ans
        , missing = correctAnswer q `difference` ans
        , wrong = ans `difference` correctAnswer q
        }

isCorrect :: Question -> Answer -> Bool
isCorrect q ans = correctAnswer q == ans

data Config = Config
    { title :: Text
    , questions :: [Question]
    , sampleAmount :: Int
    , categoryWeights :: Maybe (Map Text Int)
    }
    deriving (Show, Eq, Generic)

instance FromJSON Question
instance ToJSON Question

instance FromJSON Config
instance ToJSON Config
