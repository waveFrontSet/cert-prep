module CertPrep.Export (
  ExportFormat (..),
  ExportInput (..),
  export,
  writeExport,
) where

import System.FilePath ((</>))

import CertPrep.Export.Core
import CertPrep.Export.Json
import CertPrep.Export.Markdown

data ExportFormat = Json | Markdown

exporterFor :: ExportFormat -> Exporter
exporterFor Json = jsonExporter
exporterFor Markdown = markdownExporter

export :: ExportFormat -> ExportInput -> LByteString
export f = render (exporterFor f) . toReport

writeExport :: (MonadIO m) => FilePath -> ExportFormat -> ExportInput -> m ()
writeExport path format = writeFileLBS (path </> ("export." <> ext)) . export format
 where
  ext = toString $ extension (exporterFor format)
