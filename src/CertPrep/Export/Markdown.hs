module CertPrep.Export.Markdown (markdownExporter) where

import CertPrep.Export.Core

markdownExporter :: Exporter
markdownExporter =
  Exporter {
    extension = "md",
    render = undefined
  }
