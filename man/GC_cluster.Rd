% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/geneviewer.R
\name{GC_cluster}
\alias{GC_cluster}
\title{Modify Cluster Settings}
\usage{
GC_cluster(
  GC_chart,
  separate_strands = NULL,
  strand_spacing = NULL,
  prevent_gene_overlap = NULL,
  overlap_spacing = NULL,
  cluster = NULL,
  style = list(),
  ...
)
}
\arguments{
\item{GC_chart}{The gene chart object to be modified.}

\item{separate_strands}{Logical, indicating whether to vertically separate
forward and reverse genes.}

\item{strand_spacing}{Numeric, specifies the spacing between genes on
different strands. Used only if `separate_strands` is TRUE.}

\item{prevent_gene_overlap}{Logical, indicating whether to vertically separate
overlapping genes.}

\item{overlap_spacing}{Numeric, specifies the spacing between overlapping genes
Used only if `prevent_gene_overlap` is TRUE.}

\item{cluster}{Optional; used to specify which clusters in the chart should have tooltips.}

\item{style}{A list of CSS styles to be applied to the gene track.
Each element of the list should be a valid CSS property-value
pair. For example, list(backgroundColor = "red", color = "white").}

\item{...}{Additional arguments to be passed to the underlying functions.}
}
\value{
Returns the modified gene chart object.
}
\description{
This function can switch prevention of gene overlap on, adjust the spacing
between tracks and alter the styling of specified clusters.
}
\examples{
genes_data <- data.frame(
  start = c(1, 10, 200, 220, 600),
  end = c(10, 150, 180, 400, 400),
  name = c('Gene 1', 'Gene 2', 'Gene 3', 'Gene 4', 'Gene 5'),
  group = c('A', 'A', 'A', 'A', 'A')
)


GC_chart(genes_data, group = "group", height = "150px") \%>\%
  GC_cluster(separate_strands=TRUE, strand_spacing = 0) \%>\%
  GC_legend(FALSE)

}
