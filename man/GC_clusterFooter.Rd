% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/geneviewer.R
\name{GC_clusterFooter}
\alias{GC_clusterFooter}
\title{Add a Footer to Each Cluster in a GC Chart}
\usage{
GC_clusterFooter(
  GC_chart,
  title = NULL,
  subtitle = NULL,
  show = TRUE,
  cluster = NULL,
  subtitleFont = list(),
  titleFont = list(),
  ...
)
}
\arguments{
\item{GC_chart}{A GC chart object that the footers will be added to.}

\item{title}{Character vector or NULL. The title to be displayed in the footer.
Multiple titles can be provided for different clusters, and they will be recycled
if there are more clusters than titles. Default is NULL.}

\item{subtitle}{Character vector or NULL. Subtitles to accompany the main titles.
Default is NULL.}

\item{show}{Logical vector. Controls the visibility of each footer.
Default is TRUE for all clusters.}

\item{cluster}{Numeric or character vector specifying which clusters should have footers added or updated.
If NULL, all clusters will be updated. Default is NULL.}

\item{subtitleFont}{List, styling options for the subtitle.}

\item{titleFont}{List, styling options for the title.}

\item{...}{Additional arguments for further customization of the footers.}
}
\value{
A GC chart object with updated footer settings for each specified cluster.
}
\description{
This function allows you to add a footer to all or specific clusters within a GC chart.
You can specify titles, subtitles, and control the display and styling.
}
\examples{
genes_data <- data.frame(
  start = c(10, 90, 130, 170, 210),
  end = c(40, 120, 160, 200, 240),
  name = c('Gene 1', 'Gene 3', 'Gene 4', 'Gene 5', 'Gene 6'),
  group = c('A', 'B', 'B', 'A', 'C'),
  cluster = c(1, 1, 2, 2, 2)
)

# Add a simple footer with subtitle to all clusters
GC_chart(genes_data, cluster = "cluster", group = "group") \%>\%
  GC_clusterFooter(
    title = "Cluster Footer",
    subtitle = "Cluster subtitle"
  )
# Add styling to the title and subtitle
GC_chart(genes_data, cluster = "cluster", group = "group") \%>\%
  GC_clusterFooter(
    title = "This is a footer",
    subtitle = "Subtitle for the footer",
    spacing = 15,
    show = TRUE,
    cluster = 1,
    x = 6,
    y = -20,
    align = "center", # left / right
    spacing = 12,
    titleFont = list(
      fontSize = "12px",
      fontWeight = "bold",
      fontFamily = "sans-serif",
      fill = "black",
      cursor = "default"
    ),
    subtitleFont = list(
      fill = "grey",
      fontSize = "10px",
      fontStyle = "normal",
      fontFamily = "sans-serif",
      cursor = "default"
    )
  )

}
