module CertPrep.Export where

import Data.Aeson (encode)
import Data.Aeson.Types (ToJSON)
import Data.ByteString.Lazy (LazyByteString)

import CertPrep.Types (Answer, Question)

data ExportFormat = Json | Markdown

export :: ExportFormat -> [(Question, Answer)] -> LazyByteString
export Json = exportJson
export Markdown = undefined

data ExamJsonExportQAPair = ExamJsonExportQAPair {
  question :: Question,
  answer :: Answer
}
  deriving (Show, Eq, Generic)

instance ToJSON ExamJsonExportQAPair

exportJson :: [(Question, Answer)] -> LazyByteString
exportJson = encode . fmap (uncurry ExamJsonExportQAPair)
