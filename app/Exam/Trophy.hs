module Exam.Trophy (
    updateTrophyState,
    wrapWithTrophies,
    checkAllTrophies,
    persistTrophies,
)
where

import Data.Set qualified as Set
import Lens.Micro ((^.))

import Exam.Core
import Trophy (
    EarnedTrophies,
    FinalStatistics (..),
    TrophyDef (..),
    TrophyState (..),
    checkAfterSubmit,
    checkAtFinish,
    saveEarnedTrophies,
    trophyDefId,
 )

updateTrophyState :: Bool -> Int -> TrophyState -> TrophyState
updateTrophyState wasCorrect questionTime ts =
    ts
        { currentStreak = if wasCorrect then currentStreak ts + 1 else 0
        , lastQuestionSeconds = questionTime
        }

wrapWithTrophies :: [TrophyDef] -> ExamPhase -> ExamPhase
wrapWithTrophies [] target = target
wrapWithTrophies (t : ts) target =
    TrophyAwarded
        TrophyAwardedData
            { _awardedTrophy = t
            , _animationFrame = 0
            , _pendingTrophies = ts
            , _returnPhase = target
            }

checkAllTrophies :: TrophyState -> EarnedTrophies -> ExamCore -> [TrophyDef]
checkAllTrophies ts earned core =
    let wasCorrect = currentStreak ts > 0
        filterEarned = filter (not . (`Set.member` earned) . trophyDefId)
        submitTs = filterEarned $ checkAfterSubmit wasCorrect ts
        nextIdx = core ^. currentIndex + 1
        isLast = nextIdx >= totalQuestions core
        finishTs =
            if isLast
                then filterEarned $ checkAtFinish $ FinalStatistics (core ^. score) (totalQuestions core)
                else []
     in submitTs ++ finishTs

persistTrophies :: [TrophyDef] -> FilePath -> EarnedTrophies -> IO EarnedTrophies
persistTrophies newTrophies cp earned = do
    let trophyIds = Set.fromList (map trophyDefId newTrophies)
        newEarned = Set.union trophyIds earned
    saveEarnedTrophies cp newEarned
    pure newEarned
