#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`

#' Create a GC Chart Visualization
#'
#' Generates an interactive GC chart for genomic data.
#'
#' @param data Data frame containing genomic information or the file path to a
#'   folder containing `.gbk` files. When providing a file path, the data is
#'   loaded and processed into a data frame internally.
#' @param start Column name that indicates start positions. Default is "start".
#' @param end Column name that indicates end positions. Default is "end".
#' @param cluster Optional column name used for clustering purposes. Default is
#'   NULL.
#' @param group Column name used for gene grouping to influence color
#'   aesthetics.
#' @param strand Optional column name indicating strand orientation. Acceptable
#'   values include 1, 'forward', 'sense', or '+' to represent the forward
#'   strand, and -1, 0, 'reverse', 'antisense', "complement" or '-' to represent
#'   the reverse strand. Default is NULL, meaning strand information is not
#'   used.
#' @param width Width specification for the chart, such as '100%' or 500.
#'   Default is unspecified.
#' @param height Height specification for the chart, such as '400px' or 300.
#'   Default is unspecified.
#' @param style A list of CSS styles to be applied to the chart container. Each
#'   element of the list should be a valid CSS property-value pair. For example,
#'   list(backgroundColor = "white", border = "2px solid black"). Default is an
#'   empty list.
#' @param elementId Optional identifier string for the widget. Default is NULL.
#' @param save_button Logical, whether to include a save button to save the
#'   chart as SVG or PNG. Default is TRUE.
#'
#' @return A GC chart widget.
#'
#' @examples
#' genes_data <- data.frame(
#'   start = c(10, 50, 90, 130, 170, 210),
#'   end = c(40, 80, 120, 160, 200, 240),
#'   name = c('Gene 1', 'Gene 2', 'Gene 3', 'Gene 4', 'Gene 5', 'Gene 6'),
#'   group = c('A', 'A', 'B', 'B', 'A', 'C'),
#'   cluster = c(1, 1, 1, 2, 2, 2)
#' )
#' # Load from data.frame
#' GC_chart(genes_data, group = "group", cluster = "cluster", height = "200px") %>%
#' GC_labels("name")
#'
#' # Load from a GenBank file (or a folder of .gbk files)
#' gbk_file <- system.file("extdata", "BGC0000001.gbk", package = "geneviewer")
#' GC_chart(gbk_file, height = "200px")
#'
#' @import htmlwidgets
#' @import fontawesome
#' @export
GC_chart <- function(data, start = "start", end = "end", cluster = NULL, group = NULL, strand = NULL, width = "100%", height = "400px", style = list(), elementId = NULL, save_button = TRUE) {

  spec <- spec_args(match.call(), environment())

  # Load from .gbk files
  if (is.character(data)) {
    data <- read_gbk_cds(data, start, end)
    cluster <- "cluster"
    strand <- "strand"
    spec[c("cluster", "strand")] <- list(cluster, strand)
  }

  # The spec holds the data as given (or as read); the library prepares it as
  # prepare_data() does, which checks it here
  x <- list(spec = start_spec(data, spec))
  prepare_data(data, start, end, cluster, group, strand)

  # Determine dependencies: custom toolbar icons are Font Awesome icons
  dependencies <- if (save_button) {
    list(fontawesome::fa_html_dependency())
  } else {
    NULL
  }

  # create the widget
  htmlwidgets::createWidget(
    name = "geneviewer",
    x,
    width = width,
    height = height,
    package = "geneviewer",
    dependencies = dependencies,
    elementId = elementId,
    preRenderHook = send_spec
  )
}
