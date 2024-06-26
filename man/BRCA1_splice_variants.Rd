% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{BRCA1_splice_variants}
\alias{BRCA1_splice_variants}
\title{BRCA1 Splice Variants}
\format{
A data frame with 119 observations and 6 variables:
\describe{
  \item{ensembl_transcript_id}{Unique Ensembl transcript identifiers. A character vector.}
  \item{type}{Type of the transcript feature, indicating whether it is an "exon", or "UTR". A character vector.}
  \item{start}{The start position of the feature on the chromosome. A numeric vector.}
  \item{end}{The end position of the feature on the chromosome. A numeric vector.}
  \item{strand}{The strand orientation of the feature, with -1 indicating the reverse strand. An integer vector.}
  \item{transcript_length}{The total length of the transcript in base pairs. An integer vector.}
}
}
\source{
Derived from the Ensembl website
}
\usage{
BRCA1_splice_variants
}
\description{
This dataset contains detailed information on five different splice variants
of the BRCA1 gene. It includes data on the types of transcript features
(exon, intron, UTR), chromosomal positions, strand orientations, and
transcript lengths.
}
\keyword{datasets}
