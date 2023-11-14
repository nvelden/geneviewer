% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/GCVieweR.R
\name{GC_legend}
\alias{GC_legend}
\title{Set Legend for a Gene Chart}
\usage{
GC_legend(
  GC_chart,
  group = NULL,
  show = TRUE,
  colorScheme = NULL,
  customColors = NULL,
  backgroundColor = "white",
  labels = NULL,
  ...
)
}
\arguments{
\item{GC_chart}{The gene chart object to be modified.}

\item{group}{Optional; character or NULL, the groups to include in the legend. If NULL,
the groups are taken from the 'group' attribute of the 'GC_chart' object.}

\item{show}{Logical, whether to display the legend or not.}

\item{colorScheme}{Optional; character or NULL, the name of the color scheme to apply to the legend.
This can be one of D3.js's built-in color schemes (e.g., "schemeCategory10",
"schemeAccent", "schemeTableau10").}

\item{customColors}{Optional; list or NULL, custom colors to apply to the legend entries.}

\item{labels}{Optional; character or NULL, custom labels for the legend entries.}

\item{...}{Additional arguments to be passed to the legend configuration.}
}
\value{
Returns the gene chart object with the legend configured.
}
\description{
This function configures the legend for a gene chart. It can toggle the legend's visibility,
apply a color scheme, assign custom colors, and set custom labels for the legend entries.
}
\examples{
genes_data <- data.frame(
  start = c(10, 90, 130, 170, 210),
  stop = c(40, 120, 160, 200, 240),
  name = c('Gene 1', 'Gene 3', 'Gene 4', 'Gene 5', 'Gene 6'),
  group = c('A', 'B', 'B', 'A', 'C'),
  cluster = c('A', 'A', 'A', 'B', 'B')
)

# Assuming GC_chart is a function that creates a gene chart object
GC_chart(genes_data, cluster = "cluster", group = "group") \%>\%
  GC_legend(
    position = "top", #bottom
    orientation = "horizontal", #vertical
    x = 10,
    y = 10,
    adjustHeight = TRUE,
    labels = NULL, # c('Group A', 'Group B', 'Group C'),
    legendOptions = list(
      cursor = "pointer",
      colorScheme = NULL,
      customColors = NULL # c("red", "green", "orange")
      # Additional styles
    ),
    legendTextOptions = list(
      cursor = "pointer",
      textAnchor = "start",
      dy = ".35em",
      fontSize = "12px",
      fontFamily = "sans-serif",
      fill = "black"
      # Additional styles
    ),
  )

# Configure the legend of the gene chart
GC_chart(genes_data, cluster = "cluster", group = "group") \%>\%
  GC_legend(
    show = TRUE,
    colorScheme = 'schemeCategory10', # Example color scheme
    customColors = list('A' = 'blue', 'B' = 'green', 'C' = 'red'),
    labels = c('Group A', 'Group B', 'Group C')
  )


}