{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

module CertPrep.Export where

import Data.Aeson (encode)
import Data.Aeson.Types (ToJSON)
import Data.Map qualified as M

import CertPrep.Types (
  Answer,
  AnswerResult,
  Category,
  Question (..),
  evalAnswer,
  isCorrect,
 )

data ExportFormat = Json | Markdown
data Exporter = Exporter {
  extension :: Text,
  render :: ExportReport -> LByteString
}

exporterFor :: ExportFormat -> Exporter
exporterFor Json = jsonExporter
exporterFor Markdown = undefined

export :: ExportFormat -> ExportInput -> LByteString
export f = render (exporterFor f) . toReport

writeExport :: (MonadIO m) => FilePath -> ExportFormat -> ExportInput -> m ()
writeExport path format = writeFileLBS path . export format

jsonExporter :: Exporter
jsonExporter =
  Exporter {
    extension = "json",
    render = encode
  }

markdownExporter :: Exporter
markdownExporter =
  Exporter {
    extension = "md",
    render = undefined
  }

data ExportInput = ExportInput {
  qaPairs :: [(Question, Answer)],
  elapsedSeconds :: Int
}
  deriving (Show, Eq, Generic)

data ExportReport = ExportReport {
  totalCorrect :: Int,
  totalQuestions :: Int,
  elapsedSeconds :: Int,
  categoryStats :: Map Category CategoryStat,
  questionResults :: [QuestionResult]
}
  deriving (Show, Eq, Generic)

data CategoryStat = CategoryStat {
  category :: Category,
  correct :: Int,
  total :: Int
}
  deriving (Show, Eq, Generic)

data QuestionResult = QuestionResult {
  question :: Question,
  answer :: Answer,
  result :: AnswerResult,
  wasCorrect :: Bool
}
  deriving (Show, Eq, Generic)

instance ToJSON CategoryStat
instance ToJSON QuestionResult
instance ToJSON AnswerResult
instance ToJSON ExportReport

toReport :: ExportInput -> ExportReport
toReport ExportInput {qaPairs = qa, elapsedSeconds = eSeconds} =
  ExportReport {
    totalCorrect = length $ filter (uncurry isCorrect) qa,
    totalQuestions = length qa,
    elapsedSeconds = eSeconds,
    categoryStats = categoryMap qa,
    questionResults = fmap toQuestionResult qa
  }
 where
  categoryMap :: [(Question, Answer)] -> Map Category CategoryStat
  categoryMap = foldr go mempty
  go ::
    (Question, Answer) ->
    Map Category CategoryStat ->
    Map Category CategoryStat
  go (q@Question {category = Just cat}, a) m = case M.lookup cat m of
    Nothing -> M.insert cat (CategoryStat {category = cat, correct = score q a, total = 1}) m
    Just cs ->
      M.insert
        cat
        (cs {correct = cs.correct + score q a, total = cs.total + 1})
        m
  go _ m = m
  score q a = if isCorrect q a then 1 else 0
  toQuestionResult (q, a) =
    QuestionResult {
      question = q,
      answer = a,
      result = evalAnswer q a,
      wasCorrect = isCorrect q a
    }
