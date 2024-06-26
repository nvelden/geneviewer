% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_bed.R
\name{read_bed}
\alias{read_bed}
\title{Read BED Files}
\usage{
read_bed(path)
}
\arguments{
\item{path}{A character string specifying the directory containing BED files
or the file path to a single BED file.}
}
\value{
A data frame combining data from the BED files.
}
\description{
This function reads BED files from a specified directory or file path and
combines them into a single data frame. BED files use 0-based coordinate starts,
while this function transforms the coordinates to 1-based during import.
}
\details{
This function can read multiple BED files from a directory or a
  single BED file from a specified path. It adds a 'filename' column with the
  name of the file, and combines the data frames from all files into one.
}
\examples{
\dontrun{
# Read BED files from a directory
bed_data <- read_bed("path/to/directory")

# Read a single BED file
bed_data <- read_bed("path/to/file.bed")
}
}
