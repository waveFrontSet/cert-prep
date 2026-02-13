module Types (
    Question (..),
    Answer,
    Category,
    Trophy (..),
    TrophyCondition (..),
    TrophyIcon (..),
    defaultTrophies,
    configTrophies,
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

data TrophyCondition
    = CorrectStreakAtLeast Int
    | TotalCorrectAtLeast Int
    | FastCorrectAtMost Int
    deriving (Show, Eq, Generic)

data TrophyIcon
    = PixelRocket
    | PixelFire
    | PixelCrown
    | PixelBolt
    deriving (Show, Eq, Generic)

data Trophy = Trophy
    { trophyId :: Text
    , trophyName :: Text
    , trophyCondition :: TrophyCondition
    , trophyIcon :: TrophyIcon
    }
    deriving (Show, Eq, Generic)

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
    , trophies :: Maybe [Trophy]
    }
    deriving (Show, Eq, Generic)

defaultTrophies :: [Trophy]
defaultTrophies =
    [ Trophy
        { trophyId = "warmup-complete"
        , trophyName = "Warmup? What Warmup?"
        , trophyCondition = TotalCorrectAtLeast 1
        , trophyIcon = PixelRocket
        }
    , Trophy
        { trophyId = "hot-streak-3"
        , trophyName = "Threepeater Supreme"
        , trophyCondition = CorrectStreakAtLeast 3
        , trophyIcon = PixelFire
        }
    , Trophy
        { trophyId = "hot-streak-5"
        , trophyName = "Combo Wombo V"
        , trophyCondition = CorrectStreakAtLeast 5
        , trophyIcon = PixelCrown
        }
    , Trophy
        { trophyId = "speedrun"
        , trophyName = "Lightning Fingers"
        , trophyCondition = FastCorrectAtMost 5
        , trophyIcon = PixelBolt
        }
    ]

configTrophies :: Config -> [Trophy]
configTrophies cfg = maybe defaultTrophies id (trophies cfg)

instance FromJSON Question
instance ToJSON Question

instance FromJSON TrophyCondition
instance ToJSON TrophyCondition

instance FromJSON TrophyIcon
instance ToJSON TrophyIcon

instance FromJSON Trophy
instance ToJSON Trophy

instance FromJSON Config
instance ToJSON Config
