module Types (Question (..), Answer, isCorrect, AnswerResult (..), evalAnswer) where

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
import Data.Maybe (fromMaybe)

import GHC.Generics (Generic)

type Answer = IntSet

data Question = Question
    { questionText :: String
    , questionAnswerChoices :: [String]
    , questionCorrectAnswer :: Answer
    , questionCategory :: Maybe String
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

toLowerFirstLetter :: String -> String
toLowerFirstLetter [] = []
toLowerFirstLetter (x : xs) = toLower x : xs

customOptions :: Options
customOptions =
    defaultOptions
        { fieldLabelModifier = \s ->
            toLowerFirstLetter $ fromMaybe s (stripPrefix "question" s)
        }

instance FromJSON Question where
    parseJSON = genericParseJSON customOptions
instance ToJSON Question where
    toJSON = genericToJSON customOptions
