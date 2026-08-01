module CertPrep.Export (
  ExportFormat (..),
  ExportInput (..),
  export,
  fileExtension,
  writeExport,
) where

import System.FilePath ((<.>))

import CertPrep.Export.Core
import CertPrep.Export.Json
import CertPrep.Export.Markdown

data ExportFormat = Json | Markdown
  deriving (Show, Eq)

exporterFor :: ExportFormat -> Exporter
exporterFor Json = jsonExporter
exporterFor Markdown = markdownExporter

fileExtension :: ExportFormat -> Text
fileExtension = extension . exporterFor

export :: ExportFormat -> ExportInput -> LByteString
export f = render (exporterFor f) . toReport

{- | Write the report to @base.<ext>@, with the extension chosen by the
format. Returns the path actually written.
-}
writeExport :: (MonadIO m) => FilePath -> ExportFormat -> ExportInput -> m FilePath
writeExport base format input = do
  let path = base <.> toString (fileExtension format)
  writeFileLBS path (export format input)
  pure path
