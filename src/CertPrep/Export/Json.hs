{-# LANGUAGE OverloadedRecordDot #-}

module CertPrep.Export.Json (jsonExporter) where

import Data.Aeson (Value, encode, object, (.=))

import CertPrep.Export.Core

jsonExporter :: Exporter
jsonExporter =
  Exporter {
    extension = "json",
    render = encode . reportValue
  }

reportValue :: ExportReport -> Value
reportValue r =
  object
    [ "totalCorrect" .= r.totalCorrect,
      "totalQuestions" .= r.totalQuestions,
      "elapsedSeconds" .= r.elapsedSeconds,
      "categoryStats" .= fmap statValue (toList r.categoryStats),
      "questionResults" .= fmap resultValue r.questionResults
    ]

statValue :: CategoryStat -> Value
statValue s =
  object
    [ "category" .= s.category,
      "correct" .= s.correct,
      "total" .= s.total
    ]

resultValue :: QuestionResult -> Value
resultValue qr =
  object
    [ "question" .= qr.question,
      "answer" .= qr.answer,
      "result" .= qr.result,
      "wasCorrect" .= qr.wasCorrect
    ]
