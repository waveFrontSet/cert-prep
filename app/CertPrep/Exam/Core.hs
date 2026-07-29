{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoFieldSelectors #-}

module CertPrep.Exam.Core (
  Name (..),
  ExamCore (..),
  AnsweringData (..),
  ReviewingData (..),
  ExplainingData (..),
  ActivePhase (..),
  FinishedState (..),
  ExportDialogState (..),
  ExportingData (..),
  TrophyAwardedData (..),
  ExamPhase (..),
  AppState (..),
  ExplanationStatus (..),
  questions,
  currentIndex,
  score,
  elapsedSeconds,
  questionStartTime,
  selectedAnswers,
  focusedAnswer,
  activeCore,
  activeQuestion,
  phaseData,
  answerResult,
  lastSelected,
  finalScore,
  finalTotal,
  finalElapsed,
  finalPairs,
  exportStatus,
  exportFormats,
  exportEditor,
  exportFocus,
  exportDialog,
  exportFinished,
  awardedTrophy,
  animationFrame,
  pendingTrophies,
  returnPhase,
  examPhase,
  trophyState,
  earnedTrophies,
  totalQuestions,
  explainId,
  explanationStatus,
  nextExplainId,
  reviewingData,
  userAnswers,
)
where

import Brick.Focus qualified as F
import Brick.Widgets.Edit qualified as E
import Brick.Widgets.List qualified as L
import Data.Vector (Vector)
import Data.Vector qualified as V
import Lens.Micro ((^.))
import Lens.Micro.TH (makeLenses)
-- Qualified so the manual Show instance below can define the class
-- method, which relude's prelude hides.
import Text.Show qualified

import CertPrep.Export (ExportFormat)
import CertPrep.Trophy (EarnedTrophies, TrophyDef, TrophyState (..))
import CertPrep.Types (Answer, AnswerResult, Question)

data Name
  = AnswerChoice Int
  | SubmitButton
  | NextButton
  | TrophyDismiss
  | ExplanationViewport
  | ExportFormatChooser
  | ExportFilenameEditor
  deriving (Show, Eq, Ord)

data ExamCore = ExamCore {
  _questions :: Vector Question,
  _currentIndex :: Int,
  _score :: Int,
  _elapsedSeconds :: Int,
  _questionStartTime :: Int,
  _userAnswers :: Vector Answer
}
  deriving (Show)

makeLenses ''ExamCore

data AnsweringData = AnsweringData {
  _selectedAnswers :: IntSet,
  _focusedAnswer :: Int
}
  deriving (Show)

makeLenses ''AnsweringData

data ReviewingData = ReviewingData {
  _answerResult :: AnswerResult,
  _lastSelected :: IntSet
}
  deriving (Show)

makeLenses ''ReviewingData

data ExplanationStatus
  = ExplanationPending
  | ExplanationStreaming Text -- partial text, more chunks expected
  | ExplanationSuccess Text
  | ExplanationFailure Text
  deriving (Show, Eq)

data ExplainingData = ExplainingData {
  _explainId :: Int, -- id of the request whose events we accept
  _explanationStatus :: ExplanationStatus,
  _reviewingData :: ReviewingData
}
  deriving (Show)

makeLenses ''ExplainingData

data ActivePhase a = ActivePhase {
  _activeCore :: ExamCore,
  _activeQuestion :: Question,
  _phaseData :: a
}
  deriving (Show)

makeLenses ''ActivePhase

data FinishedState = FinishedState {
  _finalScore :: Int,
  _finalTotal :: Int,
  _finalElapsed :: Int,
  _finalPairs :: [(Question, Answer)],
  _exportStatus :: Maybe Text
}
  deriving (Show, Eq)

makeLenses ''FinishedState

data ExportDialogState = ExportDialogState {
  _exportFormats :: L.List Name ExportFormat,
  _exportEditor :: E.Editor Text Name, -- filename without extension
  _exportFocus :: F.FocusRing Name
}

makeLenses ''ExportDialogState

-- FocusRing has no Show instance, so summarize the dialog by hand.
instance Show ExportDialogState where
  show d =
    "ExportDialogState {format = "
      <> show (L.listSelectedElement (d ^. exportFormats))
      <> ", filename = "
      <> show (E.getEditContents (d ^. exportEditor))
      <> ", focus = "
      <> show (F.focusGetCurrent (d ^. exportFocus))
      <> "}"

data ExportingData = ExportingData {
  _exportDialog :: ExportDialogState,
  _exportFinished :: FinishedState
}
  deriving (Show)

makeLenses ''ExportingData

-- TrophyAwardedData and ExamPhase are mutually recursive,
-- so they must be in the same TH splice group.

data TrophyAwardedData = TrophyAwardedData {
  _awardedTrophy :: TrophyDef,
  _animationFrame :: Int,
  _pendingTrophies :: [TrophyDef],
  _returnPhase :: ExamPhase
}
  deriving (Show)

data ExamPhase
  = Answering (ActivePhase AnsweringData)
  | Reviewing (ActivePhase ReviewingData)
  | Explaining (ActivePhase ExplainingData)
  | CheckingTrophies ExamCore
  | TrophyAwarded TrophyAwardedData
  | Finished FinishedState
  | Exporting ExportingData
  deriving (Show)

makeLenses ''TrophyAwardedData

data AppState = AppState {
  _examPhase :: ExamPhase,
  _trophyState :: TrophyState,
  _earnedTrophies :: EarnedTrophies,
  _nextExplainId :: Int -- counter so stale explanation events can be told apart
}
  deriving (Show)

makeLenses ''AppState

totalQuestions :: ExamCore -> Int
totalQuestions c = V.length (c ^. questions)
