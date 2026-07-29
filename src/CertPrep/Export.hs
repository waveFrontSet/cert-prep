module CertPrep.Export (
  ExportFormat (..),
  export,
  writeExport,
) where

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
writeExport path format = writeFileLBS path . export format
