module Trophy (
    TrophyId (..),
    TrophyDef (..),
    EarnedTrophies,
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

-- Trophy definitions

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

-- | Check for trophies earned after submitting an answer.
checkAfterSubmit ::
    Bool -> Int -> Int -> EarnedTrophies -> [TrophyDef]
checkAfterSubmit wasCorrect newStreak questionSeconds alreadyEarned =
    filter (not . (`Set.member` alreadyEarned) . trophyDefId) $
        concat
            [ [firstBloodDef | wasCorrect]
            , [hatTrickDef | wasCorrect, newStreak >= 3]
            , [onFireDef | wasCorrect, newStreak >= 5]
            , [speedDemonDef | wasCorrect, questionSeconds < 5]
            ]

-- | Check for trophies earned at the end of an exam.
checkAtFinish :: Int -> Int -> EarnedTrophies -> [TrophyDef]
checkAtFinish finalScore totalQs alreadyEarned =
    filter (not . (`Set.member` alreadyEarned) . trophyDefId) $
        concat
            [ [flawlessVictoryDef | finalScore == totalQs, totalQs > 0]
            , [scholarSupremeDef | totalQs >= 10, percentage >= 90]
            , [marathonerDef | totalQs >= 20]
            ]
  where
    percentage :: Int
    percentage
        | totalQs == 0 = 0
        | otherwise = (finalScore * 100) `div` totalQs

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
