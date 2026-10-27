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

  x <- list()
  # Capture all arguments with their current values
  x$params <- as.list(environment())
  x$params$data <- NULL
  x$params <- Filter(function(y) !is.null(y) && !(is.list(y) && length(y) == 0), x$params)


  # Load from .gbk files
  if (is.character(data)) {
    data <- read_gbk_cds(data, start, end)
    cluster <- "cluster"
    strand <- "strand"
  }

  # ensure that data is a data frame
  stopifnot(is.data.frame(data))

  # Check if column names are in the data frame
  colnames_data <- colnames(data)

  if (!(start %in% colnames_data)) stop("start column not found in data")
  if (!(end %in% colnames_data)) stop("end column not found in data")

  if (!is.null(cluster) && !(cluster %in% colnames_data)) {
    stop("cluster column not found in data")
  }
  if (!is.null(group) && !(group %in% colnames_data)) {
    stop("group column not found in data")
  }
  if (!is.null(strand) && !(strand %in% colnames_data)) {
    stop("strand column not found in data")
  }

  show_legend <- if (!is.null(group)) TRUE else FALSE

  # Filter out rows with NA values in 'start' or 'end' columns
  original_nrow <- nrow(data)
  data <- data[!is.na(data[[start]]) & !is.na(data[[end]]), ]

  # Check if any rows were filtered out and issue a warning
  if (nrow(data) < original_nrow) {
    warning("Rows with NA values in 'start' or 'end' columns have been filtered out.")
  }

  # Add rowID to data
  data$rowID <- seq_len(nrow(data))

  # add start and end
  data_tmp <- data
  data$start <- data_tmp[[start]]
  data$end <- data_tmp[[end]]

  # Convert cluster to character
  if(!is.null(cluster)){
  data[[cluster]] <- as.character(data[[cluster]])
  }

  # Add strand if specified
  data <- add_strand(data, strand)

  x$data <- data
  x$group <- group
  x$cluster <- cluster
  x$graphContainer$direction <- "column"
  x$style <- style
  x$title$style <- list(width = "100%")
  x$legend <- list(
    group = group, show = show_legend,
    width = width
  )
  x$legend$style <- list(width = "100%", backgroundColor = style$backgroundColor)
  x$saveButton <- save_button


  if (is.null(cluster)) {
    clusters <- "cluster"
  } else {
    clusters <- unique(as.character(data[[cluster]]))
  }

  for (clust in clusters) {
    # Subset data for the current cluster
    if (is.null(cluster)) {
      subset_data <- data
    } else {
      subset_data <- data[data[[cluster]] == clust, ]
    }

    # add cluster
    subset_data$cluster <- clust

    # Data
    x$series[[clust]]$clusterName <- clust
    x$series[[clust]]$data <- subset_data
    x$series[[clust]]$cluster <- list()
    if (is.character(height) && grepl("%$", height)) {
      cluster_height <- compute_size(height, length(clusters))
    } else {
      cluster_height <- get_relative_height(height, height) / length(clusters)
    }
    x$series[[clust]]$container <- list(height = cluster_height, width = width)
    x$series[[clust]]$container$style <- list(width = "100%", backgroundColor = style$backgroundColor)
    x$series[[clust]]$genes <- list(group = group, show = TRUE)
    x$series[[clust]]$sequence <- list(show = TRUE)
    x$series[[clust]]$scale <- list(xMin = min(subset_data$start, subset_data$end), xMax = max(subset_data$start, subset_data$end))
    x$series[[clust]]$transcript <- list(group = group, show = FALSE)
    x$series[[clust]]$labels <- list(group = group, show = TRUE)
    x$series[[clust]]$coordinates <- list(show = FALSE)
    x$series[[clust]]$scaleBar <- list()
    x$series[[clust]]$footer <- list()
    x$series[[clust]]$clusterLabel <- list()
    x$series[[clust]]$clusterTitle <- list()
    x$series[[clust]]$annotations <- list()
    x$series[[clust]]$trackMouse <- list(show = FALSE)

    # Blast specific settings
    if(all(c("BlastP", "protein_id") %in% colnames_data) &&
       any(subset_data$BlastP == subset_data$protein_id, na.rm = TRUE) &&
       all(subset_data$BlastP %in% c(subset_data$protein_id, "No Hit", NA))){
      # The query cluster: its genes are only hits of themselves
      formatter <-
        "<b>{protein_id}</b><br>
        <b>Query gene</b><br>
        <b>Location:</b> {start} - {end}<br>"
    } else if(all(c("BlastP", "protein_id") %in% colnames_data)){
      formatter <-
        "<b>{protein_id}</b><br>
        <b>BlastP hit with:</b> {BlastP}<br>
        <b>Identity:</b> {identity}%<br>
        <b>Similarity:</b> {similarity}%<br>
        <b>Location:</b> {start} - {end}<br>"
    } else {
      formatter <-
        "<b>Start:</b> {start}<br><b>End:</b> {end}"
    }
    x$series[[clust]]$tooltip <- list(
      show = TRUE,
      formatter = formatter
    )
  }

  # Determine dependencies
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
    preRenderHook = render_theme
  )
}
