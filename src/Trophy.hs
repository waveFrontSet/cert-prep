{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Trophy (
    TrophyId (..),
    TrophyDef (..),
    TrophyState (..),
    EarnedTrophies,
    FinalStatistics (..),
    checkAfterSubmit,
    checkAtFinish,
    trophyFilePath,
    loadEarnedTrophies,
    saveEarnedTrophies,
) where

import Data.Aeson (FromJSON, ToJSON, eitherDecodeStrict)
import Data.Aeson qualified as Aeson
import Data.ByteString qualified as BS
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import GHC.Generics (Generic)
import System.Directory (
    XdgDirectory (XdgConfig),
    createDirectoryIfMissing,
    doesFileExist,
    getXdgDirectory,
 )
import System.FilePath (takeDirectory, (</>))

data TrophyId
    = FirstBlood
    | HatTrick
    | OnFire
    | SpeedDemon
    | FlawlessVictory
    | ScholarSupreme
    | Marathoner
    deriving (Show, Eq, Ord, Enum, Bounded, Generic)

instance FromJSON TrophyId
instance ToJSON TrophyId

type EarnedTrophies = Set TrophyId

data TrophyDef = TrophyDef
    { trophyDefId :: TrophyId
    , trophyName :: Text
    , trophyDesc :: Text
    , trophyIcon :: [Text]
    }
    deriving (Show, Eq)

data TrophyState = TrophyState
    { currentStreak :: Int
    , lastQuestionSeconds :: Int
    }
    deriving (Show, Eq)

data FinalStatistics = FinalStatistics
    { nCorrectQuestions :: Int
    , nQuestions :: Int
    }

data TrophyCheckTime = WhenReviewing | WhenFinishing

type family CondInput (t :: TrophyCheckTime) where
    CondInput 'WhenReviewing = TrophyState
    CondInput 'WhenFinishing = FinalStatistics

data TrophyData (t :: TrophyCheckTime) = TrophyData
    { trophyDef :: TrophyDef
    , trophyCond :: CondInput t -> Bool
    }

-- Trophy definitions
currentStreakGte :: Int -> TrophyState -> Bool
currentStreakGte n ts = currentStreak ts >= n

firstBloodDef :: TrophyDef
firstBloodDef =
    TrophyDef
        { trophyDefId = FirstBlood
        , trophyName = "First Blood"
        , trophyDesc = "Answer your first question correctly"
        , trophyIcon =
            [ "    \9608\9608    "
            , "   \9608\9604\9604\9608   "
            , "  \9608\9608\9617\9617\9608\9608  "
            , " \9608\9608\9617\9608\9608\9617\9608\9608 "
            , "  \9608\9608\9617\9617\9608\9608  "
            , "   \9608\9604\9604\9608   "
            , "    \9608\9608    "
            ]
        }

firstBlood :: TrophyData 'WhenReviewing
firstBlood = TrophyData{trophyDef = firstBloodDef, trophyCond = currentStreakGte 1}

hatTrickDef :: TrophyDef
hatTrickDef =
    TrophyDef
        { trophyDefId = HatTrick
        , trophyName = "Hat Trick"
        , trophyDesc = "Get 3 correct answers in a row"
        , trophyIcon =
            [ "  \9608\9608\9608\9608\9608\9608\9608\9608  "
            , " \9608\9608\9618\9618\9618\9618\9618\9618\9608\9608 "
            , "\9608\9608\9608\9608\9608\9608\9608\9608\9608\9608\9608\9608"
            , "\9608\9608\9617\9617\9617\9617\9617\9617\9617\9617\9608\9608"
            , " \9608\9608\9608\9608\9608\9608\9608\9608\9608\9608 "
            , "   \9608\9608\9608\9608\9608\9608   "
            , "   \9608\9608\9608\9608\9608\9608   "
            ]
        }

hatTrick :: TrophyData 'WhenReviewing
hatTrick = TrophyData{trophyDef = hatTrickDef, trophyCond = currentStreakGte 3}

onFireDef :: TrophyDef
onFireDef =
    TrophyDef
        { trophyDefId = OnFire
        , trophyName = "On Fire!"
        , trophyDesc = "Get 5 correct answers in a row"
        , trophyIcon =
            [ "    \9617\9608\9608\9617    "
            , "   \9617\9608\9608\9608\9608\9617   "
            , "  \9618\9608\9608\9608\9608\9608\9608\9618  "
            , " \9618\9608\9608\9608\9608\9608\9608\9608\9608\9618 "
            , " \9619\9608\9608\9608\9608\9608\9608\9608\9608\9619 "
            , "  \9619\9608\9608\9608\9608\9608\9608\9619  "
            , "   \9608\9608\9608\9608\9608\9608   "
            ]
        }

onFire :: TrophyData 'WhenReviewing
onFire = TrophyData{trophyDef = onFireDef, trophyCond = currentStreakGte 5}

speedDemonDef :: TrophyDef
speedDemonDef =
    TrophyDef
        { trophyDefId = SpeedDemon
        , trophyName = "Speed Demon"
        , trophyDesc = "Answer correctly in under 5 seconds"
        , trophyIcon =
            [ "   \9604\9608\9608\9608\9608\9604   "
            , "  \9608\9608\9600\9600\9600\9600\9608\9608  "
            , " \9608\9608  \9608\9608  \9608\9608 "
            , " \9608\9608  \9600\9608  \9608\9608 "
            , " \9608\9608      \9608\9608 "
            , "  \9608\9608\9604\9604\9604\9604\9608\9608  "
            , "   \9600\9608\9608\9608\9608\9600   "
            ]
        }

questionSecondsLt :: Int -> TrophyState -> Bool
questionSecondsLt n ts = lastQuestionSeconds ts < n

speedDemon :: TrophyData 'WhenReviewing
speedDemon = TrophyData{trophyDef = speedDemonDef, trophyCond = questionSecondsLt 5}

flawlessVictoryDef :: TrophyDef
flawlessVictoryDef =
    TrophyDef
        { trophyDefId = FlawlessVictory
        , trophyName = "Flawless Victory"
        , trophyDesc = "Achieve a perfect score"
        , trophyIcon =
            [ "  \9604\9608\9608\9608\9608\9608\9608\9604  "
            , " \9608\9608 \9600\9608\9608\9600 \9608\9608 "
            , " \9600\9608\9604\9608\9608\9608\9608\9604\9608\9600 "
            , "  \9608\9608\9608\9608\9608\9608\9608\9608  "
            , "   \9608\9608\9608\9608\9608\9608   "
            , "    \9608\9608\9608\9608    "
            , "   \9608\9608\9608\9608\9608\9608   "
            ]
        }

flawlessVictory :: TrophyData 'WhenFinishing
flawlessVictory =
    TrophyData
        { trophyDef = flawlessVictoryDef
        , trophyCond = \fs -> nQuestions fs > 0 && nCorrectQuestions fs == nQuestions fs
        }

scholarSupremeDef :: TrophyDef
scholarSupremeDef =
    TrophyDef
        { trophyDefId = ScholarSupreme
        , trophyName = "Scholar Supreme"
        , trophyDesc = "Score 90%+ with 10 or more questions"
        , trophyIcon =
            [ "   \9608\9608\9608\9608\9608\9608   "
            , "  \9608\9608\9617\9617\9617\9617\9608\9608  "
            , " \9608\9608\9617\9608\9608\9608\9608\9617\9608\9608 "
            , " \9608\9608\9617\9608\9608\9608\9608\9617\9608\9608 "
            , " \9608\9608\9617\9617\9617\9617\9617\9617\9608\9608 "
            , "  \9608\9608\9608\9608\9608\9608\9608\9608\9608\9608"
            , "   \9608\9608\9608\9608\9608\9608\9608\9608 "
            ]
        }

scholarSupreme :: TrophyData 'WhenFinishing
scholarSupreme =
    TrophyData
        { trophyDef = scholarSupremeDef
        , trophyCond = \fs -> nQuestions fs >= 10 && (nCorrectQuestions fs * 100 `div` nQuestions fs) >= 90
        }

marathonerDef :: TrophyDef
marathonerDef =
    TrophyDef
        { trophyDefId = Marathoner
        , trophyName = "Marathoner"
        , trophyDesc = "Complete a session with 20+ questions"
        , trophyIcon =
            [ "     \9604\9608\9604     "
            , "    \9608\9608\9608\9608\9608    "
            , "   \9608\9608\9608\9608\9608\9608\9608   "
            , "  \9608\9608\9608\9608\9608\9608\9608\9608\9608  "
            , " \9608\9608\9617\9617\9608\9608\9608\9608\9608\9617\9617 "
            , "    \9608\9608\9608\9608\9608    "
            , "   \9608\9608\9608\9608\9608\9608\9608   "
            ]
        }

marathoner :: TrophyData 'WhenFinishing
marathoner = TrophyData{trophyDef = marathonerDef, trophyCond = (>= 20) . nQuestions}

-- | Check for trophies earned after submitting an answer.
checkAfterSubmit :: Bool -> TrophyState -> [TrophyDef]
checkAfterSubmit wasCorrect ts =
    map trophyDef $
        filter
            ((&&) wasCorrect . flip trophyCond ts)
            [ firstBlood
            , hatTrick
            , onFire
            , speedDemon
            ]

-- | Check for trophies earned at the end of an exam.
checkAtFinish :: FinalStatistics -> [TrophyDef]
checkAtFinish fs =
    map trophyDef $
        filter
            (`trophyCond` fs)
            [flawlessVictory, scholarSupreme, marathoner]

-- Persistence

sanitizePath :: FilePath -> FilePath
sanitizePath = map sanitizeChar
  where
    sanitizeChar '/' = '_'
    sanitizeChar '\\' = '_'
    sanitizeChar ':' = '_'
    sanitizeChar c = c

trophyFilePath :: FilePath -> IO FilePath
trophyFilePath configPath = do
    dir <- getXdgDirectory XdgConfig "cert-prep"
    pure $ dir </> "trophies" </> sanitizePath configPath ++ ".json"

loadEarnedTrophies :: FilePath -> IO EarnedTrophies
loadEarnedTrophies configPath = do
    path <- trophyFilePath configPath
    exists <- doesFileExist path
    if not exists
        then pure Set.empty
        else do
            bytes <- BS.readFile path
            case eitherDecodeStrict bytes of
                Left _ -> pure Set.empty
                Right trophies -> pure trophies

saveEarnedTrophies :: FilePath -> EarnedTrophies -> IO ()
saveEarnedTrophies configPath trophies = do
    path <- trophyFilePath configPath
    createDirectoryIfMissing True (takeDirectory path)
    BS.writeFile path (BS.toStrict $ Aeson.encode trophies)
