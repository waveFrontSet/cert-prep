module Exam.TransitionSpec (spec) where

import Brick.Focus qualified as F
import Brick.Widgets.List qualified as L
import Lens.Micro
import Test.Hspec

import CertPrep.Exam.Core
import CertPrep.Exam.Transition (
  applyExplainEvent,
  cancelExport,
  exportBaseName,
  finishExam,
  finishExport,
  openExportDialog,
  selectedExportFormat,
  stepExplanation,
  toExportInput,
  travelToQuestion,
 )
import CertPrep.Explanations (ExplainError (..), ExplainEvent (..), renderExplainError)
import CertPrep.Export (ExportFormat (..), ExportInput (ExportInput))
import Generators (mkQuestion)

spec :: Spec
spec = do
  let
    q1 = mkQuestion "Q1" ["A", "B", "C"] [0] Nothing
    qs =
      [ q1,
        mkQuestion "Q2" ["X", "Y"] [1] Nothing,
        mkQuestion "Q3" ["M", "N"] [0] Nothing
      ]
    mkCore idx scr =
      ExamCore {
        _questions = fromList qs,
        _currentIndex = idx,
        _score = scr,
        _elapsedSeconds = 42,
        _questionStartTime = 0,
        _userAnswers = fromList [fromList [0], fromList [1]]
      }
    mkReviewing idx =
      ActivePhase {
        _activeCore = mkCore idx 0,
        _activeQuestion = fromMaybe q1 (qs !!? idx),
        _phaseData =
          ReviewingData {
            _answerResult = error "answerResult is never forced in this test",
            _lastSelected = mempty
          }
      }
  describe "travelToQuestion" $ do
    it "travels to previous answered question" $
      let ap = travelToQuestion (-1) (mkReviewing 1)
       in (ap ^. (activeCore . currentIndex)) `shouldBe` 0
    it "travels to next answered question" $
      let ap = travelToQuestion 1 (mkReviewing 0)
       in (ap ^. (activeCore . currentIndex)) `shouldBe` 1
    it "doesn't travel beyond the last answered question" $
      let ap = travelToQuestion 1 (mkReviewing 1)
       in (ap ^. (activeCore . currentIndex)) `shouldBe` 1
    it "doesn't travel beyond the first answered question" $
      let ap = travelToQuestion (-1) (mkReviewing 0)
       in (ap ^. (activeCore . currentIndex)) `shouldBe` 0

  describe "stepExplanation" $ do
    it "starts streaming on the first chunk" $
      stepExplanation (ExplainChunk "Hel") ExplanationPending
        `shouldBe` ExplanationStreaming "Hel"
    it "appends subsequent chunks" $
      stepExplanation (ExplainChunk "lo") (ExplanationStreaming "Hel")
        `shouldBe` ExplanationStreaming "Hello"
    it "finalizes the streamed text on done" $
      stepExplanation ExplainDone (ExplanationStreaming "Hello")
        `shouldBe` ExplanationSuccess "Hello"
    it "treats done without any chunk as an empty response" $
      stepExplanation ExplainDone ExplanationPending
        `shouldBe` ExplanationFailure (renderExplainError ExplainEmptyResponse)
    it "fails mid-stream on error, discarding partial text" $
      stepExplanation (ExplainFailed (ExplainHttpError "boom")) (ExplanationStreaming "Hel")
        `shouldBe` ExplanationFailure (renderExplainError (ExplainHttpError "boom"))
    it "ignores chunks after success" $
      stepExplanation (ExplainChunk "junk") (ExplanationSuccess "Hello")
        `shouldBe` ExplanationSuccess "Hello"
    it "ignores done after failure" $
      stepExplanation ExplainDone (ExplanationFailure "nope")
        `shouldBe` ExplanationFailure "nope"

  describe "applyExplainEvent" $ do
    let mkExplaining rid status =
          Explaining
            ActivePhase {
              _activeCore = mkCore 0 0,
              _activeQuestion = q1,
              _phaseData =
                ExplainingData {
                  _explainId = rid,
                  _explanationStatus = status,
                  _reviewingData =
                    ReviewingData {
                      _answerResult = error "answerResult is never forced in this test",
                      _lastSelected = mempty
                    }
                }
            }
        statusOf (Explaining ap) = Just (ap ^. phaseData . explanationStatus)
        statusOf _ = Nothing
    it "applies events carrying the current request id" $
      statusOf (applyExplainEvent 1 (ExplainChunk "Hi") (mkExplaining 1 ExplanationPending))
        `shouldBe` Just (ExplanationStreaming "Hi")
    it "drops events from an abandoned request" $
      statusOf (applyExplainEvent 1 (ExplainChunk "Hi") (mkExplaining 2 ExplanationPending))
        `shouldBe` Just ExplanationPending
    it "leaves other phases untouched" $
      statusOf (applyExplainEvent 1 ExplainDone (Reviewing (mkReviewing 0)))
        `shouldBe` Nothing

  describe "finishExam" $ do
    let fs = finishExam (mkCore 1 1)
    it "zips questions with the user's answers" $
      fs ^. finalPairs `shouldBe` zip qs [fromList [0], fromList [1]]
    it "starts without an export status" $
      fs ^. exportStatus `shouldBe` Nothing

  describe "export dialog" $ do
    let fs = finishExam (mkCore 1 1)
        dlg = case openExportDialog "export-x" fs of
          Exporting ed' -> ed' ^. exportDialog
          _ -> error "expected Exporting"
        ed = ExportingData {_exportDialog = dlg, _exportFinished = fs}
    it "opens with the given filename in the editor" $
      exportBaseName dlg `shouldBe` "export-x"
    it "opens with Markdown selected" $
      selectedExportFormat dlg `shouldBe` Markdown
    it "opens with the filename editor focused" $
      F.focusGetCurrent (dlg ^. exportFocus) `shouldBe` Just ExportFilenameEditor
    it "carries the finished state through" $
      case openExportDialog "export-x" fs of
        Exporting ed' -> ed' ^. exportFinished `shouldBe` fs
        _ -> expectationFailure "expected Exporting"
    it "selects Json when the format list moves down" $
      selectedExportFormat (dlg & exportFormats %~ L.listMoveDown) `shouldBe` Json
    it "builds the export input from the finished state" $
      toExportInput fs `shouldBe` ExportInput (zip qs [fromList [0], fromList [1]]) 42
    it "cancelling returns to Finished unchanged" $
      case cancelExport ed of
        Finished fs' -> fs' `shouldBe` fs
        _ -> expectationFailure "expected Finished"
    it "finishing records the status message" $
      case finishExport "Exported to export-x.md" ed of
        Finished fs' -> fs' ^. exportStatus `shouldBe` Just "Exported to export-x.md"
        _ -> expectationFailure "expected Finished"
