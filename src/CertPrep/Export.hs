module CertPrep.Export where

import CertPrep.Types (Answer, Question)

data ExportFormat = Json | Markdown

export :: ExportFormat -> [(Question, Answer)] -> Text
export = undefined
