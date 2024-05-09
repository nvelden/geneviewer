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
#' @param width Width specification for the chart, such as '100\%' or 500.
#'   Default is unspecified.
#' @param height Height specification for the chart, such as '400px' or 300.
#'   Default is unspecified.
#' @param style A list of CSS styles to be applied to the chart container. Each
#'   element of the list should be a valid CSS property-value pair. For example,
#'   list(backgroundColor = "white", border = "2px solid black"). Default is an
#'   empty list.
#' @param elementId Optional identifier string for the widget. Default is NULL.
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
#' # Load from folder containing .gbk files
#' # file_path <- "~/path/to/folder/"
#' # GC_chart(file_path) %>%
#'
#' @import htmlwidgets
#' @export
GC_chart <- function(data, start = "start", end = "end", cluster = NULL, group = NULL, strand = NULL, width = "100%", height = "400px", style = list(), elementId = NULL) {

  # Load from .gbk files
  if (is.character(data)) {
    gbk <- geneviewer::read_gbk(data) # Adjust based on actual function call
    data <- geneviewer::gbk_features_to_df(gbk, feature = "CDS", keys = c("protein_id", "region", "translation"))
    data <- data[!is.na(data[[start]]) & !is.na(data[[end]]), ]
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

  x <- list()

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
  x$links <- NULL
  x$group <- group
  x$cluster <- cluster
  x$graphContainer$direction <- "column"
  x$style <- style
  x$title$style <- list(width = "100%")
  x$legend <- list(
    group = group, show = show_legend, position = "bottom",
    width = width
  )
  x$legend$style <- list(width = "100%", backgroundColor = style$backgroundColor)


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
    x$series[[clust]]$container <- list(height = get_relative_height(height, height) / length(clusters), width = width)
    x$series[[clust]]$container$style <- list(width = "100%", backgroundColor = style$backgroundColor)
    x$series[[clust]]$genes <- list(group = group, show = TRUE)
    x$series[[clust]]$scale <- list(xMin = min(subset_data$start, subset_data$end), xMax = max(subset_data$start, subset_data$end))
    x$series[[clust]]$labels <- list(group = group, show = TRUE)
    x$series[[clust]]$coordinates <- list(show = FALSE)
    x$series[[clust]]$scaleBar <- list()
    x$series[[clust]]$footer <- list()
    x$series[[clust]]$clusterLabel <- list()
    x$series[[clust]]$clusterTitle <- list()
    x$series[[clust]]$sequence <- list(show = TRUE)
    x$series[[clust]]$annotations <- list()
    x$series[[clust]]$trackMouse <- list(show = FALSE)

    if(all(c("BlastP", "protein_id") %in% colnames_data)){
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
    x$series[[clust]]$tooltip <-
      formatter <- list(
        show = TRUE,
        formatter = formatter
      )
  }

  # create the widget
  htmlwidgets::createWidget(
    name = "geneviewer",
    x,
    width = width,
    height = height,
    package = "geneviewer",
    elementId = elementId
  )
}

#' Add Title to a GC_chart
#'
#' Modify the cluster title and subtitle of specified clusters within a GC chart and adjust
#' the display settings.
#'
#' @param GC_chart A GC chart object.
#' @param title Character vector. Titles to set for the clusters.
#' @param subtitle Character vector. Subtitles to set for the clusters.
#' @param show Logical. Whether to display the title. Default is TRUE.
#' @param height Character. Height for the title (e.g., "50px").
#' @param style A list of CSS styles to be applied to the chart container. Each
#'   element of the list should be a valid CSS property-value pair. For example,
#'   list(backgroundColor = "white", border = "2px solid black").
#'   Default is an empty list.
#' @param titleFont List. Settings for the title font.
#' @param subtitleFont List. Settings for the subtitle font.
#' @param ... Additional customization arguments for title and subtitle.
#'
#' @return Updated GC chart with new title settings.
#'
#' @examples
#' genes_data <- data.frame(
#'   start = c(10, 50, 90, 130, 170, 210),
#'   end = c(40, 80, 120, 160, 200, 240),
#'   name = c('Gene 1', 'Gene 2', 'Gene 3', 'Gene 4', 'Gene 5', 'Gene 6'),
#'   group = c('A', 'A', 'B', 'B', 'A', 'C'),
#'   cluster = c(1, 1, 1, 2, 2, 2)
#' )
#'
#' # Basic usage
#' GC_chart(genes_data, cluster ="cluster", group = "group", height = "400px") %>%
#' GC_labels("name") %>%
#' GC_title(
#'   title = "Cluster 1 Data",
#'   subtitle = "Detailed View",
#'   show = TRUE
#' )
#'
#' # Customizing title style
#' GC_chart(genes_data, cluster = "cluster", group = "group", height = "500px") %>%
#'   GC_labels("name") %>%
#'   GC_title(
#'     title = "Cluster 1 Data",
#'     subtitle = "Detailed View",
#'     show = TRUE,
#'     height = "50px",
#'     cluster = 1,
#'     x = 0,
#'     y = 25, # height / 2
#'     align = "center",
#'     spacing = 20,
#'     style = list(
#'       backgroundColor = "#0000"
#'       # Any other CSS styles
#'     ),
#'     titleFont = list(
#'       fontSize = "16px",
#'       fontStyle = "normal",
#'       fontWeight = "bold",
#'       textDecoration = "normal",
#'       fontFamily = "sans-serif",
#'       cursor = "default",
#'       fill = "black"
#'       # Any other CSS styles
#'     ),
#'     subtitleFont = list(
#'       fontSize = "14px",
#'       fontStyle = "normal",
#'       fontWeight = "bold",
#'       textDecoration = "normal",
#'       fontFamily = "sans-serif",
#'       cursor = "default",
#'       fill = "black"
#'       # Any other CSS styles
#'     )
#'   )
#'
#' @export
GC_title <- function(
    GC_chart,
    title = NULL,
    subtitle = NULL,
    style = list(),
    subtitleFont = list(),
    titleFont = list(),
    show = TRUE,
    height = NULL,
    ...
) {

  if (!show || is.null(title) && is.null(subtitle)) {
    return(GC_chart)
  }

  if (is.null(height)) {
    if (!is.null(title) && !is.null(subtitle)) {
      height <- 50
    } else if (!is.null(title) || !is.null(subtitle)) {
      height <- 30
    }
  }

  # Capture arguments and filter out NULL or empty values
  options <- Filter(function(x) !is.null(x) && length(x) > 0, list(
      title = title,
      subtitle = subtitle,
      subtitleFont = subtitleFont,
      titleFont = titleFont,
      style = style,
      height = height,
      show = show,
      ...
    ))

  # Merge new options with existing options
  if (is.null(GC_chart$x$title)) {
    GC_chart$x$title <- options
  } else {
    GC_chart$x$title <- modifyList(GC_chart$x$title, options)
  }

  return(GC_chart)
}

#' Update cluster Title of a GC Chart Cluster
#'
#' Modify the cluster title and subtitle of specified clusters within a GC chart and adjust
#' the display settings.
#'
#' @param GC_chart A GC chart object.
#' @param title Character vector. Titles to set for the clusters.
#' @param subtitle Character vector. Subtitles to set for the clusters.
#' @param show Logical. Whether to display the title. Default is TRUE.
#' @param height Character. Height for the title (e.g., "40px").
#' @param cluster Numeric or character vector. Clusters in the GC chart to update.
#' @param titleFont List. Settings for the title font.
#' @param subtitleFont List. Settings for the subtitle font.
#' @param ... Additional customization arguments for title and subtitle.
#'
#' @return Updated GC chart with new title settings.
#'
#' @examples
#' genes_data <- data.frame(
#'   start = c(10, 50, 90, 130, 170, 210),
#'   end = c(40, 80, 120, 160, 200, 240),
#'   name = c('Gene 1', 'Gene 2', 'Gene 3', 'Gene 4', 'Gene 5', 'Gene 6'),
#'   group = c('A', 'A', 'B', 'B', 'A', 'C'),
#'   cluster = c(1, 1, 1, 2, 2, 2)
#' )
#'
#' # Basic usage
#' GC_chart(genes_data, cluster ="cluster", group = "group", height = "400px") %>%
#' GC_labels("name") %>%
#' GC_clusterTitle(
#'   title = "Cluster 1 Data",
#'   subtitle = "Detailed View",
#'   show = TRUE,
#'   cluster = 1
#' )
#'
#' # Customizing title style
#' GC_chart(genes_data, cluster = "cluster", group = "group", height = "400px") %>%
#'   GC_labels("name") %>%
#'   GC_clusterTitle(
#'     title = "Cluster 1 Data",
#'     subtitle = "Detailed View",
#'     show = TRUE,
#'     cluster = 1,
#'     x = 0,
#'     y = 5,
#'     align = "center",
#'     spacing = 20,
#'     titleFont = list(
#'       fontSize = "16px",
#'       fontStyle = "normal",
#'       fontWeight = "bold",
#'       textDecoration = "normal",
#'       fontFamily = "sans-serif",
#'       cursor = "default",
#'       fill = "black"
#'       # Any other CSS styles
#'     ),
#'     subtitleFont = list(
#'       fontSize = "14px",
#'       fontStyle = "normal",
#'       fontWeight = "bold",
#'       textDecoration = "normal",
#'       fontFamily = "sans-serif",
#'       cursor = "default",
#'       fill = "black"
#'       # Any other CSS styles
#'     )
#'   )
#' @export
GC_clusterTitle <- function(
    GC_chart,
    title = NULL,
    subtitle = NULL,
    subtitleFont = list(),
    titleFont = list(),
    show = TRUE,
    height = NULL,
    cluster = NULL,
    ...) {
  if (!show) {
    return(GC_chart)
  }

  # Capture ... arguments
  dots <- list(...)

  # Update the GC_chart object with title and options for each cluster
  clusters <- getUpdatedClusters(GC_chart, cluster)

  for (i in seq_along(clusters)) {
    # Default options
    options <- list(
      title = title[(i - 1) %% length(title) + 1],
      subtitle = subtitle[(i - 1) %% length(subtitle) + 1],
      subtitleFont = subtitleFont,
      titleFont = titleFont,
      show = show
    )

    # Add arguments to options
    for (name in names(dots)) {
      options[[name]] <- dots[[name]][(i - 1) %% length(dots[[name]]) + 1]
    }

    # Set height for each cluster
    GC_chart$x$series[[clusters[i]]]$grid$margin$top <- height[(i - 1) %% length(height) + 1]

    # Set title options for each cluster
    GC_chart$x$series[[clusters[i]]]$clusterTitle <- options
  }
  return(GC_chart)
}

#' Update Sequence Display of a GC Chart Cluster
#'
#' Modify the sequence display and break markers of specified clusters within a GC chart.
#'
#' This function allows customization of the sequence line and break markers in a GC chart.
#' It offers options to adjust the sequence line (`sequenceStyle`) and break markers (`markerStyle`).
#' The `y` parameter can be used to set the vertical position of the sequence.
#'
#' @param GC_chart A GC chart object.
#' @param show Logical, whether to display the sequence (default is TRUE).
#' @param y Vertical position of the sequence line (default is 50).
#' @param cluster Numeric or character vector specifying clusters to update.
#' @param sequenceStyle A list of styling options for the sequence line.
#' @param markerStyle A list of styling options for the sequence break markers.
#' @param ... Additional customization arguments for sequence display.
#'
#' @return An updated GC chart with modified sequence display settings.
#'
#' @examples
#' genes_data <- data.frame(
#'   start = c(10, 90, 130, 170, 210),
#'   end = c(40, 120, 160, 200, 240),
#'   name = c('Gene 1', 'Gene 3', 'Gene 4', 'Gene 5', 'Gene 6'),
#'   group = c('A', 'B', 'B', 'A', 'C'),
#'   cluster = c(1, 1, 2, 2, 2)
#' )
#'
#' # Basic usage
#' GC_chart(genes_data, cluster ="cluster", group = "group", height = "200px") %>%
#' GC_labels("name") %>%
#' GC_sequence(show = TRUE, y = 50, cluster = NULL)
#'
#' # Customize sequence and marker styles
#' GC_chart(genes_data, cluster="cluster", group = "group", height = "200px") %>%
#'   GC_scale(hidden = TRUE, scale_breaks = TRUE) %>%
#'   GC_sequence(
#'     start = NULL,
#'     end = NULL,
#'     sequenceStyle = list(
#'       stroke = "blue",
#'       strokeWidth = 1
#'       # Any other CSS style
#'     ),
#'     markerStyle = list(
#'       stroke = "blue",
#'       strokeWidth = 1,
#'       gap = 3,
#'       tiltAmount = 5
#'       # Any other CSS style
#'     )
#'   ) %>%
#'   GC_legend(FALSE)
#'
#' @export
GC_sequence <- function(
    GC_chart,
    show = TRUE,
    cluster = NULL,
    y = 50,
    sequenceStyle = list(),
    markerStyle = list(),
    ...
) {

  # Capture ... arguments
  dots <- list(...)

  # Update the GC_chart object with title and options for each cluster
  clusters <- getUpdatedClusters(GC_chart, cluster)

  for(i in seq_along(clusters)){

    # Default options
    options <- list(
      show = show[(i-1) %% length(show) + 1],
      y = y[(i-1) %% length(y) + 1],
      sequenceStyle = sequenceStyle,
      markerStyle = markerStyle
    )

    # Add ... arguments to options
    for(name in names(dots)) {
      options[[name]] <- dots[[name]][(i-1) %% length(dots[[name]]) + 1]
    }

    # Set sequence options for each cluster
    GC_chart$x$series[[clusters[i]]]$sequence <- options

  }

  return(GC_chart)
}

#' Update Grid Display of a GC Chart Cluster
#'
#' Modify the grid display of specified clusters within a GC chart. This
#' function allows users to adjust the margins, width, and height of the grid
#' for each cluster.
#'
#' @param GC_chart A GC chart object.
#' @param margin A list specifying top, right, bottom, and left margins.
#' @param width Numeric or character. Width of the grid. If numeric, will be
#' considered as percentage.
#' @param height Numeric. Height of the grid.
#' @param direction Character. Layout direction of the grid, either "column"
#' (default) for vertical or "row" for horizontal.
#' @param cluster Numeric or character vector. Clusters in the GC chart to update.
#'
#' @return Updated GC chart with new grid display settings.
#' @importFrom utils modifyList
#' @examples
#' genes_data <- data.frame(
#'   start = c(10, 90, 130, 170, 210),
#'   end = c(40, 120, 160, 200, 240),
#'   name = c('Gene 1', 'Gene 3', 'Gene 4', 'Gene 5', 'Gene 6'),
#'   group = c('A', 'B', 'B', 'A', 'C'),
#'   cluster = c(1, 1, 2, 2, 2)
#' )
#'
#' # Set Margin of clusters
#' GC_chart(genes_data, cluster ="cluster", group = "group", height = "200px") %>%
#' GC_grid(margin = list(left = "50px", right = "0px"))
#'
#' # Set height of a specific cluster
#' GC_chart(genes_data, cluster ="cluster", group = "group", height = "200px") %>%
#' GC_grid(height = "120px", cluster = 2)
#'
#' @export
GC_grid <- function(
    GC_chart,
    margin = NULL,
    width = NULL,
    height = NULL,
    direction = "column",
    cluster = NULL
) {

  all_clusters <- names(GC_chart$x$series)
  update_clusters <- getUpdatedClusters(GC_chart, cluster)
  chart_height <- GC_chart$height

  if(!is.null(width) && is.null(cluster)){
    GC_chart$width <- get_relative_height(GC_chart$width, width)
  }

  if(is.null(cluster) && direction == "row"){
    GC_chart$x$graphContainer$direction <- "row"
  }

  # Update margins
  if(!is.null(margin) && is.null(cluster)){

    paddingTop <- if (!is.null(margin[["top"]])) get_relative_height(chart_height, margin[["top"]]) else 0
    paddingBottom <- if (!is.null(margin[["bottom"]])) get_relative_height(chart_height, margin[["bottom"]]) else 0

    margin[["top"]] <- NULL
    margin[["bottom"]] <- NULL

    # Update left and right margins
    GC_chart$x$legend$margin <- if (is.null(GC_chart$x$legend$margin)) margin else utils::modifyList(GC_chart$x$legend$margin, margin)
    GC_chart$x$title$margin <- if (is.null(GC_chart$x$title$margin)) margin else utils::modifyList(GC_chart$x$title$margin, margin)

    # Update top and bottom margins
    if(paddingTop > 0){

      GC_chart$x$style[["paddingTop"]] <- paste0(as.character(paddingTop), "px")
      GC_chart$height <- "100%"
      for (i in seq_along(all_clusters)) {
        cluster_name <- update_clusters[i]
        current_height <- get_relative_height(GC_chart$x$series[[cluster_name]]$container$height, GC_chart$x$series[[cluster_name]]$container$height)
        GC_chart$x$series[[cluster_name]]$container$height <- current_height - (paddingTop / length(all_clusters))
      }
    }

    if(paddingBottom > 0){
      GC_chart$height <- "100%"
      GC_chart$x$style[["paddingBottom"]] <- paste0(as.character(paddingBottom), "px")
      for (i in seq_along(all_clusters)) {
        cluster_name <- update_clusters[i]
        current_height <- get_relative_height(GC_chart$x$series[[cluster_name]]$container$height, GC_chart$x$series[[cluster_name]]$container$height)
        GC_chart$x$series[[cluster_name]]$container$height <- GC_chart$x$series[[cluster_name]]$container$height - (paddingBottom / length(all_clusters))
      }
    }

  }

  # Udpate cluster dimensions
  for (i in seq_along(update_clusters)) {
    cluster_name <- update_clusters[i]

    # Update left and right margins if provided
    if (!is.null(margin)) {
        default_margin <- GC_chart$x$series[[cluster_name]]$container$margin
        GC_chart$x$series[[cluster_name]]$container$margin <- if (is.null(default_margin)) margin else utils::modifyList(default_margin, margin)
    }

    # Update width if provided
    if (!is.null(width) && !is.null(cluster)) {
      current_width <- width[(i-1) %% length(width) + 1]
      current_width <- get_relative_height(GC_chart$width, current_width)
      GC_chart$x$series[[cluster_name]]$container$style$width <- current_width
      GC_chart$x$series[[cluster_name]]$container$width <- current_width
    }

    # Update height if provided
    if (!is.null(height)) {
      if(!is.null(cluster)){
      current_height <- height[(i-1) %% length(height) + 1]
      current_height <- get_relative_height(chart_height, current_height)
      GC_chart$x$series[[cluster_name]]$container$height <- current_height
      } else {
        GC_chart$x$series[[cluster_name]]$container$height <- compute_size(height, length(all_clusters))
      }
    }
  }

  return(GC_chart)
}

#' Update Scale of a GC Chart Cluster
#'
#' Modify the scale settings for specified clusters within a GC chart.
#'
#' @param GC_chart A GC chart object.
#' @param cluster Numeric or character vector specifying clusters in the GC
#' chart to update.
#' @param start Numeric vector indicating the starting point for the scale.
#' Default is NULL.
#' @param end Numeric vector indicating the end point for the scale.
#' Default is NULL.
#' @param padding Numeric value or percentage string indicating the padding on
#' either side of the scale. The value can be a number or a string in the
#' format of '2\%'. Default is 2.
#' @param hidden Logical flag indicating whether the axis is hidden. Default is
#' FALSE.
#' @param breaks List specifying settings for the scale breaks. Default is an
#' empty list ().
#' @param axis_position Character string indicating the type of the axis ('top' or
#' bottom'). Default is 'bottom'.
#' @param axis_type Character string indicating the type of the axis ('position' or
#' 'range'). Default is 'position'.
#' @param tickValues Numeric vector or NULL, custom tick values to be used at
#' the top of the cluster. If NULL, the default tick values are used.
#' @param reverse Logical flag indicating whether to reverse the scale for the
#' corresponding cluster. Default is FALSE.
#' @param scale_breaks Logical flag indicating if scale breaks should be
#' employed. Default is FALSE.
#' @param scale_break_threshold Numeric value indicating the threshold
#' percentage for determining scale breaks. Default is 20.
#' @param scale_break_padding Numeric value indicating the padding on either
#' side of a scale break. Default is 1.
#' @param ticksCount Numeric value indicating the number of ticks on the scale.
#' Default is 10.
#' @param ticksFormat Format for tick labels; depends on axis_type, defaulting
#' to ",.0f" for 'position' or ".2s" for 'range' when NULL.
#' @param y Numeric value from 1 to 100 indicating the y-position of the x-axis.
#' Default is NULL.
#' @param tickStyle List specifying the style for the ticks.
#' @param textStyle List specifying the style for the tick text.
#' @param lineStyle List specifying the style for the axis line.
#' @param ... Additional arguments for scale settings.
#'
#' @return Updated GC chart with new scale settings.
#'
#' @examples
#' genes_data <- data.frame(
#'   start = c(100, 1000, 2000),
#'   end = c(150, 1500, 2500),
#'   name = c('Gene 4', 'Gene 5', 'Gene 6'),
#'   group = c('B', 'A', 'C'),
#'   cluster = c(2, 2, 2)
#' )
#' #Example usage with all custom options
#' GC_chart(genes_data, cluster = "cluster", group = "group", height = "200px") %>%
#'   GC_scale(
#'     start = 1,
#'     end = 2600,
#'     padding = 2,
#'     hidden = FALSE,
#'     reverse = FALSE,
#'     axis_position = "bottom",
#'     # breaks = list(
#'     #  list(start = 160, end = 900),
#'     #  list(start = 1600, end = 1900)
#'     # ),
#'     # tickValues = c(1, 2600),
#'     scale_breaks = TRUE,
#'     scale_break_threshold = 20,
#'     scale_break_padding = 1,
#'     ticksCount = 10,
#'     ticksFormat = ",.0f",
#'     y = NULL,
#'     tickStyle =
#'       list(
#'         stroke = "grey",
#'         strokeWidth = 1,
#'         lineLength = 6
#'         # Any other CSS styles
#'       ),
#'     textStyle =
#'       list(
#'         fill = "black",
#'         fontSize = "10px",
#'         fontFamily = "Arial",
#'         cursor = "default"
#'         # Any other CSS styles
#'       ),
#'     lineStyle = list(
#'       stroke = "grey",
#'       strokeWidth = 1
#'       # Any other CSS styles
#'     )
#'   ) %>%
#'   GC_legend(FALSE)
#' @export
GC_scale <- function(GC_chart,
                     cluster = NULL,
                     start = NULL,
                     end = NULL,
                     padding = 2,
                     hidden = FALSE,
                     breaks = list(),
                     tickValues = NULL,
                     reverse = FALSE,
                     axis_position = "bottom",
                     axis_type = "position", # range
                     y = NULL,
                     scale_breaks = FALSE,
                     scale_break_threshold = 20,
                     scale_break_padding = 1,
                     ticksCount = 10,
                     ticksFormat = NULL,
                     tickStyle = list(),
                     textStyle = list(),
                     lineStyle = list(),
                     ...) {
  # Capture ... arguments
  dots <- list(...)

  xMin <- NULL
  xMax <- NULL

  if(axis_type == "range"){
    if(is.null(ticksFormat)){
      ticksFormat <- ".2s"
    }
  data <-  adjust_to_range(GC_chart$x$data, cluster = GC_chart$x$cluster)
  GC_chart$x$data <- data
  xMin <- 0
  xMax <- max(data$start, data$end)
  }

  # Update the GC_chart object with title and options for each cluster
  clusters <- getUpdatedClusters(GC_chart, cluster)

  for (i in seq_along(clusters)) {

    subset_data <- GC_chart$x$series[[clusters[i]]]$data
    start_idx <- (i - 1) %% length(start) + 1
    stop_idx <- (i - 1) %% length(end) + 1
    reverse_idx <- (i - 1) %% length(reverse) + 1

    if (axis_type == "range" && !hidden) {
      subset_data <- data[data[[GC_chart$x$cluster]] == clusters[i], ]
      GC_chart$x$series[[clusters[i]]]$data <- subset_data

      # Set hidden to TRUE for all but the last cluster
      if (i < length(clusters)) {
        hidden_current <- TRUE
      } else {
        hidden_current <- hidden
      }
    } else {
      hidden_current <- hidden
    }

    # Compute scale breaks if required
    if (scale_breaks) {
      breaks_data <-
        get_scale_breaks(subset_data,
                         threshold_percentage = scale_break_threshold,
                         padding = scale_break_padding)
    } else {
      breaks_data <- breaks
    }

    # Default options
    options <- list(
      start = start[start_idx],
      end = end[stop_idx],
      xMin = xMin,
      xMax = xMax,
      padding = padding,
      hidden = hidden_current,
      breaks = breaks_data,
      tickValues = tickValues,
      reverse = reverse[reverse_idx],
      axisPosition = axis_position,
      axisType = axis_type,
      scale_breaks = scale_breaks,
      scale_break_threshold = scale_break_threshold,
      scale_break_padding = scale_break_padding,
      ticksCount = ticksCount,
      ticksFormat = ticksFormat,
      y = y,
      tickStyle = tickStyle,
      textStyle = textStyle,
      lineStyle = lineStyle
    )

    # Set scale options for each cluster
    GC_chart$x$series[[clusters[i]]]$scale <- options

  }

  return(GC_chart)
}

#' Update Scale Bar of a GC Chart Cluster
#'
#' Modify the scale bar settings for specified clusters within a GC chart.
#'
#' @param GC_chart A GC chart object.
#' @param show Logical. Whether to show the scale bar.
#' @param cluster Numeric or character vector. Clusters in the GC chart to update.
#' @param scaleBarLineStyle List of style options for the scale bar line.
#' @param scaleBarTickStyle List of style options for the scale bar ticks.
#' @param labelStyle List of style options for the scale bar label.
#' @param ... Additional arguments for scale bar settings.
#'
#' @return Updated GC chart with new scale bar settings.
#'
#' @examples
#' genes_data <- data.frame(
#'  start = c(1000, 9000, 13000, 17000, 21000),
#'  end = c(4000, 12000, 16000, 20000, 24000),
#'  name = c('Gene 1', 'Gene 3', 'Gene 4', 'Gene 5', 'Gene 6'),
#'  group = c('A', 'B', 'B', 'A', 'C'),
#'  cluster = c(1, 1, 2, 2, 2)
#' )
#'
#' # Set scale bar for individual clusters
#' GC_chart(genes_data, cluster ="cluster", group = "group") %>%
#' GC_scaleBar(cluster = 1, title = "1 kb", scaleBarUnit = 1000) %>%
#' GC_scaleBar(cluster = 2, title = "2 kb", scaleBarUnit = 2000)
#'
#' # Style scale bar
#' GC_chart(genes_data, cluster ="cluster", group = "group", height = "400px") %>%
#'   GC_scaleBar(
#'     title = "1kb",
#'     scaleBarUnit = 1000,
#'     cluster = NULL,
#'     x = 0,
#'     y = 50,
#'     labelStyle = list(
#'       labelPosition =  "left",
#'       fontSize = "10px",
#'       fontFamily = "sans-serif",
#'       fill = "red", # Text color
#'       cursor = "default"
#'       # Any other CSS style for the label
#'     ),
#'     textPadding = -2,
#'     scaleBarLineStyle = list(
#'       stroke = "black",
#'       strokeWidth = 1
#'       # Any other CSS style for the line
#'     ),
#'     scaleBarTickStyle = list(
#'       stroke = "black",
#'       strokeWidth = 1
#'       # Any other CSS style for the tick
#'     )
#'   )
#'
#' @export
GC_scaleBar <- function(
    GC_chart,
    show = TRUE,
    cluster = NULL,
    scaleBarLineStyle = list(),
    scaleBarTickStyle = list(),
    labelStyle = list(),
    ...
) {

  if (!show) {
    return(GC_chart)
  }

  # Capture ... arguments
  dots <- list(...)

  # Update the GC_chart object with title and options for each cluster
  clusters <- getUpdatedClusters(GC_chart, cluster)

  for(i in seq_along(clusters)){

    # Default options
    options <- list(
      show = show[(i-1) %% length(show) + 1],
      scaleBarLineStyle = scaleBarLineStyle,
      scaleBarTickStyle = scaleBarTickStyle,
      labelStyle = labelStyle
    )

    # Add ... arguments to options
    for(name in names(dots)) {
      options[[name]] <- dots[[name]][(i-1) %% length(dots[[name]]) + 1]
    }

    # Set scaleBar options for each cluster
    GC_chart$x$series[[clusters[i]]]$scaleBar <- options

  }

  return(GC_chart)
}

#' Set or Update Cluster Labels for a GC Chart
#'
#' This function allows you to set or update the labels for specified clusters
#' within a GC chart. It provides flexibility in terms of the title, visibility,
#' width, position, and other additional customization options.
#'
#' @param GC_chart A GC chart object.
#' @param title Character vector. The title for the cluster label. Default is NULL.
#' @param show Logical. Whether to show the cluster label. Default is TRUE.
#' @param width Character. The width of the cluster label. Default is "100px".
#' @param cluster Numeric or character vector. Clusters in the GC chart to update.
#' Default is NULL.
#' @param position Character. Position of the label, either "left" or "right".
#' Default is "left".
#' @param wrapLabel Logical. Indicates whether the label should be wrapped.
#' Default is TRUE.
#' @param wrapOptions List. Specifies the wrapping options.
#' Default is an empty List.
#' @param ... Additional customization arguments for the cluster label,
#' such as 'fontSize', 'fontStyle', 'fontWeight', 'fontFamily', 'cursor', etc.
#'
#'
#' @return Updated GC chart with new or modified cluster labels.
#'
#' @examples
#' genes_data <- data.frame(
#'   start = c(10, 90, 130, 170, 210),
#'   end = c(40, 120, 160, 200, 240),
#'   name = c('Gene 1', 'Gene 3', 'Gene 4', 'Gene 5', 'Gene 6'),
#'   group = c('A', 'B', 'B', 'A', 'C'),
#'   cluster = c(1, 1, 2, 2, 2)
#' )
#'
#' # Set cluster labels
#' GC_chart(genes_data, cluster ="cluster", group = "group", height = "200px") %>%
#' GC_clusterLabel(title = unique(genes_data$cluster))
#'
#' # Set label for a specific cluster
#' GC_chart(genes_data, cluster ="cluster", group = "group", height = "200px") %>%
#' GC_clusterLabel(title = "Cluster 1", cluster = 1)
#'
#' # Style labels
#' GC_chart(genes_data, cluster ="cluster", group = "group", height = "200px") %>%
#'   GC_clusterLabel(
#'     title = c("Cluster 1", "Cluster 2"),
#'     width = "100px",
#'     x = 0,
#'     y = 0,
#'     position = "left",
#'     wrapLabel = TRUE,
#'     wrapOptions = list(
#'       dyAdjust = 0,
#'       lineHeightEms = 1.05,
#'       lineHeightSquishFactor =  1,
#'       splitOnHyphen =  TRUE,
#'       centreVertically = TRUE
#'      ),
#'     fontSize = "12px",
#'     fontStyle = "normal",
#'     fontWeight = "bold",
#'     fontFamily = "sans-serif",
#'     cursor = "default"
#'   )
#'
#' @export
GC_clusterLabel <- function(
    GC_chart,
    title = NULL,
    show = TRUE,
    width = "100px",
    cluster = NULL,
    position = "left",
    wrapLabel = TRUE,
    wrapOptions = list(),
    ...
) {

  if (!show) {
    return(GC_chart)
  }

  cluster_column <- GC_chart$x$cluster

  if(!is.null(cluster_column) && is.null(title)){
    title <- unique(GC_chart$x$data[[GC_chart$x$cluster]])
  }

  # Capture ... arguments
  dots <- list(...)

  # Update the GC_chart object with title and options for each cluster
  clusters <- getUpdatedClusters(GC_chart, cluster)

  for(i in seq_along(clusters)){

    # Get the current position for this cluster
    currentPosition <- position[(i-1) %% length(position) + 1]

    # Default options
    options <- list(
      title = title[(i-1) %% length(title) + 1],
      show = show[(i-1) %% length(show) + 1],
      wrapLabel = wrapLabel,
      wrapOptions = wrapOptions,
      position = currentPosition
    )

    # Add ... arguments to options
    for(name in names(dots)) {
      options[[name]] <- dots[[name]][(i-1) %% length(dots[[name]]) + 1]
    }

    # Set clusterLabel options for each cluster
    GC_chart$x$series[[clusters[i]]]$clusterLabel <- options

  }

  # Set width for each cluster based on position
  if (currentPosition == "left") {
    GC_chart <- GC_grid(GC_chart, margin = list(left = width[(i-1) %% length(width) + 1]))
    #GC_chart$x$series[[clusters[i]]]$grid$margin$left <- width[(i-1) %% length(width) + 1]
  } else if (currentPosition == "right") {
    GC_chart <- GC_grid(GC_chart, margin = list(right = width[(i-1) %% length(width) + 1]))
  }

  return(GC_chart)
}

#' Add a Footer to Each Cluster in a GC Chart
#'
#' This function allows you to add a footer to all or specific clusters within a GC chart.
#' You can specify titles, subtitles, and control the display and styling.
#'
#' @param GC_chart A GC chart object that the footers will be added to.
#' @param title Character vector or NULL. The title to be displayed in the footer.
#'        Multiple titles can be provided for different clusters, and they will be recycled
#'        if there are more clusters than titles. Default is NULL.
#' @param subtitle Character vector or NULL. Subtitles to accompany the main titles.
#'        Default is NULL.
#' @param height Character vector or NULL. The height of the footer, which can vary
#'        between clusters. Default is NULL.
#' @param show Logical vector. Controls the visibility of each footer.
#'        Default is TRUE for all clusters.
#' @param cluster Numeric or character vector specifying which clusters should have footers added or updated.
#'        If NULL, all clusters will be updated. Default is NULL.
#' @param subtitleFont List, styling options for the subtitle.
#' @param titleFont List, styling options for the title.
#' @param ... Additional arguments for further customization of the footers.
#'
#' @return A GC chart object with updated footer settings for each specified cluster.
#'
#' @examples
#' genes_data <- data.frame(
#'   start = c(10, 90, 130, 170, 210),
#'   end = c(40, 120, 160, 200, 240),
#'   name = c('Gene 1', 'Gene 3', 'Gene 4', 'Gene 5', 'Gene 6'),
#'   group = c('A', 'B', 'B', 'A', 'C'),
#'   cluster = c(1, 1, 2, 2, 2)
#' )
#'
#' # Add a simple footer with subtitle to all clusters
#' GC_chart(genes_data, cluster = "cluster", group = "group", height = "300px") %>%
#'   GC_clusterFooter(
#'     title = "Cluster Footer",
#'     subtitle = "Cluster subtitle"
#'   )
#' # Add styling to the title and sub title
#' GC_chart(genes_data, cluster = "cluster", group = "group", height = "300px") %>%
#'   GC_clusterFooter(
#'     title = "This is a footer",
#'     subtitle = "Subtitle for the footer",
#'     height = "15px",
#'     spacing = 15,
#'     show = TRUE,
#'     cluster = 1,
#'     x = 6,
#'     y = -20,
#'     align = "center", # left / right
#'     spacing = 12,
#'     titleFont = list(
#'       fontSize = "12px",
#'       fontWeight = "bold",
#'       fontFamily = "sans-serif",
#'       fill = "black",
#'       cursor = "default"
#'     ),
#'     subtitleFont = list(
#'       fill = "grey",
#'       fontSize = "10px",
#'       fontStyle = "normal",
#'       fontFamily = "sans-serif",
#'       cursor = "default"
#'     )
#'   )
#'
#' @export
GC_clusterFooter <- function(
    GC_chart,
    title = NULL,
    subtitle = NULL,
    height = NULL,
    show = TRUE,
    cluster = NULL,
    subtitleFont = list(),
    titleFont = list(),
    ...
) {

  if (!show) {
    return(GC_chart)
  }

  # Capture ... arguments
  dots <- list(...)

  # Update the GC_chart object with title and options for each cluster
  clusters <- getUpdatedClusters(GC_chart, cluster)

  for(i in seq_along(clusters)){

    # Determine the height based on title and subtitle
    if (!is.null(title) && !is.null(subtitle)) {
      currentHeight <- "20px"
    } else {
      currentHeight <- "10px"
    }

    # Override with user-specified height if provided
    if (!is.null(height)) {
      currentHeight <- height[(i-1) %% length(height) + 1]
    }

    # Default options
    options <- list(
      title = title[(i-1) %% length(title) + 1],
      subtitle = subtitle[(i-1) %% length(subtitle) + 1],
      show = show[(i-1) %% length(show) + 1],
      subtitleFont = subtitleFont,
      titleFont = titleFont
    )

    # Add ... arguments to options
    for(name in names(dots)) {
      options[[name]] <- dots[[name]][(i-1) %% length(dots[[name]]) + 1]
    }

    # Set footer options for each cluster
    GC_chart$x$series[[clusters[i]]]$footer <- options

    # Set height for each cluster
    GC_chart$x$series[[clusters[i]]]$grid$bottom <- currentHeight

  }

  return(GC_chart)
}

#' Add Labels to Each Cluster in a GC Chart
#'
#' This function adds labels to each cluster within a GC chart. It provides the
#' option to show or hide the labels and supports customization of label properties.
#' It can automatically pick up group names as labels from the `GC_chart` object if not provided.
#'
#' @param GC_chart A `GC chart` object to which labels will be added.
#' @param label Character vector, logical, or NULL. Specific labels for the clusters.
#'        If NULL and `GC_chart` has group names, those will be used as labels.
#' @param show Logical; controls the visibility of labels. Default is `TRUE`.
#' @param cluster Numeric or character vector or NULL; specifies which clusters should be labeled.
#'        If NULL, labels will be applied to all clusters. Default is NULL.
#' @param itemStyle List, a list of styles to apply to individual items in the chart.
#' @param ... Additional arguments for further customization of the labels.
#'
#' @return A `GC chart` object with updated label settings for each specified cluster.
#'
#' @examples
#' genes_data <- data.frame(
#'   start = c(10, 90, 130, 170, 210),
#'   end = c(40, 120, 160, 200, 240),
#'   name = c('Gene 1', 'Gene 3', 'Gene 4', 'Gene 5', 'Gene 6'),
#'   group = c('A', 'B', 'B', 'A', 'C'),
#'   cluster = c(1, 1, 2, 2, 2)
#' )
#'
#' # Add labels to all clusters
#' GC_chart(genes_data, cluster = "cluster", group = "group", height = "200px") %>%
#' GC_labels()
#'
#' # Add labels and styling
#' GC_chart(genes_data, cluster = "cluster", group = "group", height = "200px") %>%
#' GC_labels(
#'  label = "group",
#'  show = TRUE,
#'  x = 0,
#'  y = 50,
#'  dy = "-1.2em",
#'  dx = "0em",
#'  rotate = 0,
#'  adjustLabels = TRUE, # Rotate labels to prevent overlap
#'  fontSize = "12px",
#'  fontStyle = "italic",
#'  fill = "black",
#'  fontFamily = "sans-serif",
#'  textAnchor = "middle",
#'  cursor = "default",
#' )
#'
#' # Alter style of a specific label
#' GC_chart(genes_data, cluster = "cluster", group = "group", height = "200px") %>%
#' GC_labels(label = "group") %>%
#' GC_labels(
#'   cluster = 1,
#'   itemStyle = list(
#'     list(index = 0, fill = "red", fontSize = "14px", fontWeight = "bold")
#'   )
#' )
#'
#' @export
GC_labels <- function(
    GC_chart,
    label = NULL,
    show = TRUE,
    cluster = NULL,
    itemStyle = list(),
    ...
) {

  if (!show) {
    return(GC_chart)
  }

  if (is.logical(label) && length(label) == 1) {
    show <- label
    label <- NULL
  }

  if (is.null(label) && is.null(GC_chart$x$group)){
    stop("Please define labels")
  }

  if (is.null(label) && !is.null(GC_chart$x$group)) {
    label <- GC_chart$x$group
  }

  if (!(label %in% names(GC_chart$x$data))) {
    stop("label column not found in data")
  }

  # Update the GC_chart object with title and options for each cluster
  clusters <- getUpdatedClusters(GC_chart, cluster)

  for(i in seq_along(clusters)){

    # Default options
    options <- Filter(function(x) !is.null(x) && length(x) > 0, list(
      label = label[(i-1) %% length(label) + 1],
      show = show[(i-1) %% length(show) + 1],
      itemStyle = itemStyle,
      ...
    ))

    # Merge new options with existing options
    if (is.null(GC_chart$x$series[[clusters[i]]]$labels)) {
      GC_chart$x$series[[clusters[i]]]$labels <- options
    } else {
      GC_chart$x$series[[clusters[i]]]$labels <- modifyList(GC_chart$x$series[[clusters[i]]]$labels, options)
    }
  }

  return(GC_chart)
}

#' Modify Coordinates in a GC Chart
#'
#' This function updates a GC chart by modifying the coordinates settings. It
#' allows for showing or hiding tick values, applying custom tick values for the
#' top and bottom axes, and supports several other customizations for specific
#' or all clusters in the chart.
#'
#' @param GC_chart The GC chart object to be modified.
#' @param show Logical, whether to show the tick values or not. Can be a single
#'   value or a vector.
#' @param tickValuesTop Numeric vector or NULL, custom tick values to be used at
#'   the top of the cluster. If NULL, the default tick values are used.
#' @param tickValuesBottom Numeric vector or NULL, custom tick values to be used
#'   at the bottom of the cluster. If NULL, the default tick values are used.
#' @param cluster Numeric or character vector or NULL; specifies which clusters
#' to generate coordinates for.
#'        If NULL, labels will be applied to all clusters. Default is NULL.
#' @param tickStyle List, styling options for the ticks.
#' @param textStyle List, styling options for the text.
#'   the coordinate modifications. If NULL, applies to all clusters.
#' @param ... Additional arguments to be passed to the coordinate options.
#'
#' @return Returns the GC chart object with updated coordinates.
#'
#' @examples
#' genes_data <- data.frame(
#'   start = c(10, 90, 130, 170, 210),
#'   end = c(40, 120, 160, 200, 240),
#'   name = c('Gene 1', 'Gene 2', 'Gene 3', 'Gene 4', 'Gene 5'),
#'   cluster = c(1, 1, 2, 2, 2)
#' )
#'
#' # Add coordinates to all clusters
#' GC_chart(genes_data, cluster = "cluster", group = "name", height = "200px") %>%
#' GC_coordinates()
#'
#' # Modify coordinates of a specific cluster
#' GC_chart(genes_data, cluster = "cluster", group = "name", height = "200px") %>%
#' GC_coordinates() %>%
#' GC_coordinates(
#'   cluster = 2,
#'   show = TRUE,
#'   tickValuesTop = c(130, 170, 210, 240),
#'   tickValuesBottom = c(160, 200),
#'   rotate = -45,
#'   yPositionTop = 55,
#'   yPositionBottom = 45,
#'   overlapThreshold = 20,
#'   tickStyle = list(
#'   stroke = "black",
#'   strokeWidth = 1,
#'   lineLength = 6
#'   ),
#'   textStyle = list(
#'   fill = "black",
#'   fontSize = "12px",
#'   fontFamily = "Arial",
#'   cursor = "default"
#'   )
#' )
#'
#' @export
GC_coordinates <- function(
    GC_chart,
    show = TRUE,
    tickValuesTop = NULL,
    tickValuesBottom = NULL,
    tickStyle = list(),
    textStyle = list(),
    cluster = NULL,
    ...
) {

  if (!show) {
    return(GC_chart)
  }

  # Capture ... arguments
  dots <- list(...)

  # Update the GC_chart object with title and options for each cluster
  clusters <- getUpdatedClusters(GC_chart, cluster)

  for(i in seq_along(clusters)){

    # Default options
    options <- list(
      show = show[(i-1) %% length(show) + 1],
      tickStyle = tickStyle,
      textStyle = textStyle
    )

    # Add ... arguments to options
    for(name in names(dots)) {
      options[[name]] <- dots[[name]][(i-1) %% length(dots[[name]]) + 1]
    }

    # Set coordinates options for each cluster
    GC_chart$x$series[[clusters[i]]]$coordinates <- options

    # Add tickvalues for each cluster
    GC_chart$x$series[[clusters[i]]]$coordinates$tickValuesTop <- tickValuesTop
    GC_chart$x$series[[clusters[i]]]$coordinates$tickValuesBottom <- tickValuesBottom

  }

  # Check if prevent_gene_overlap is called last
  if (!is.null(GC_chart$x$prevent_gene_overlap_called) && GC_chart$x$prevent_gene_overlap_called) {
    warning("Separating strands must be called after setting genes, labels or coordinates for proper effect.")
  }

  return(GC_chart)
}


#' Modify Gene Characteristics within a Chart
#'
#' This function updates a gene chart with specific characteristics for genes
#' based on the given parameters. It can show/hide genes, apply a color scheme,
#' assign custom colors, filter by cluster, and accept additional options.
#'
#' @param GC_chart The gene chart object to be modified.
#' @param group Character or NULL, groups to show or hide in the chart. If NULL,
#'   the group is taken from the chart object.
#' @param marker Character or NULL, type of marker to represent genes on the chart.
#' Allowed values are 'arrow', 'boxarrow', 'box', 'cbox', and 'rbox'.
#' @param marker_size Character or NULL, size category of the marker
#' ('small', 'medium', 'large').
#' @param show Logical, whether to show the genes or not.
#' @param colorScheme Character or NULL, the name of the color scheme to use.
#' @param customColors List or NULL, custom colors to apply to the genes.
#' @param cluster Numeric or character, the specific cluster to filter genes by.
#' @param itemStyle List, a list of styles to apply to individual items in the
#'   chart.
#' @param ... Additional arguments to be passed to the gene options.
#'
#' @return Returns the modified gene chart object.
#'
#' @examples
#' genes_data <- data.frame(
#'   start = c(10, 90, 130, 170, 210),
#'   end = c(40, 120, 160, 200, 240),
#'   name = c('Gene 1', 'Gene 3', 'Gene 4', 'Gene 5', 'Gene 6'),
#'   group = c('A', 'B', 'B', 'A', 'C'),
#'   cluster = c(1, 1, 2, 2, 2)
#' )
#'
#' # Change the appearance of a specific gene cluster
#' GC_chart(genes_data, cluster = "cluster", group = "group", height = "200px") %>%
#'   GC_genes(
#'     group = "group",
#'     show = TRUE,
#'     marker = "arrow",
#'     marker_size = "medium",
#'     colorScheme = NULL, # One of D3.js build in colorSchemes
#'                         # (eg. "schemeCategory10",
#'                         # "schemeAccent", "schemeTableau10")
#'     customColors = NULL, # A vector of color names
#'     prevent_overlap = FALSE,
#'     gene_overlap_spacing = 40,
#'     cluster = 1, # Specify a specific cluster
#'     x = 1,
#'     y = 50,
#'     stroke = "black",
#'     strokeWidth = 1,
#'     arrowheadWidth = NULL,
#'     arrowheadHeight = NULL,
#'     arrowHeight = NULL,
#'     markerHeight = NULL # overwrites marker_size
#'    )
#'
#' # Change the appearance of a specific gene
#' GC_chart(genes_data, cluster = "cluster", group = "group", height = "200px") %>%
#' GC_genes(
#'   cluster = 2,
#'   itemStyle = list(list(index = 2, fill = "red", stroke = "blue")
#'   )
#'  )
#'
#' @export
GC_genes <- function(
    GC_chart,
    group = NULL,
    marker = NULL,
    marker_size = NULL,
    show = TRUE,
    colorScheme = NULL,
    customColors = NULL,
    cluster = NULL,
    itemStyle = list(),
    ...
) {

  if (is.logical(group) && length(group) == 1) {
    show <- group
    group <- NULL
  }

  if (is.null(group) && is.null(GC_chart$x$group) && is.null(marker)){
    stop("Please define a group")
  }

  if (is.null(group) && !is.null(GC_chart$x$group)){
    group <- GC_chart$x$group
  }

  if (!is.null(group) && !(group %in% names(GC_chart$x$data))) {
    stop("group column not found in data")
  }

  # Update the GC_chart object with title and options for each cluster
  clusters <- getUpdatedClusters(GC_chart, cluster)

  for(i in seq_along(clusters)){

    # Default options
    options <- Filter(function(x) !is.null(x) && length(x) > 0, list(
      group = group[(i-1) %% length(group) + 1],
      marker = marker[(i-1) %% length(marker) + 1],
      markerSize = marker_size,
      show = show[(i-1) %% length(show) + 1],
      colorScheme = colorScheme,
      customColors = customColors,
      itemStyle = itemStyle,
      ...
    ))

    GC_chart$x$series[[clusters[i]]]$genes <- options

  }

  # Check if prevent_gene_overlap is called last
  if (!is.null(GC_chart$x$prevent_gene_overlap_called) && GC_chart$x$prevent_gene_overlap_called) {
    warning("Preventing gene overlap must be called after setting genes, labels or coordinates for proper effect.")
  }

  return(GC_chart)
}

#' Update Color Scheme in Gene Chart
#'
#' This function updates the color scheme of the legend and genes in a gene chart.
#'
#' @param GC_chart The gene chart object to be modified.
#' @param colorScheme Optional; character or NULL, the name of a predefined
#' color scheme to apply to the genes.Acceptable values include D3.js's built-in
#' color schemes like "schemeCategory10", "schemeAccent", "schemeTableau10".
#' @param customColors Either NULL, a list of color values, or a named list of
#' color values.
#'
#' @return Returns the gene chart object with updated color settings for the genes.
#'
#' @examples
#' genes_data <- data.frame(
#'   start = c(10, 50, 90, 130, 170, 210),
#'   end = c(40, 80, 120, 160, 200, 240),
#'   name = c('Gene 1', 'Gene 2', 'Gene 3', 'Gene 4', 'Gene 5', 'Gene 6'),
#'   group = c('A', 'A', 'B', 'B', 'A', 'C'),
#'   cluster = c(1, 1, 1, 2, 2, 2)
#' )
#'
#' GC_chart(genes_data, cluster = "cluster", group = "group", height = "100px") %>%
#'   GC_color(colorScheme = "schemeCategory10")
#'
#' GC_chart(genes_data, cluster = "cluster", group = "group", height = "100px") %>%
#'   GC_color(customColors = c("red", "orange", "green"))
#'
#' GC_chart(genes_data, cluster = "cluster", group = "group", height = "100px") %>%
#'   GC_color(customColors = list(A = "yellow", B = "pink", C =  "purple"))
#'
#'
#' @export
GC_color <- function(
    GC_chart,
    colorScheme = NULL,
    customColors = NULL
) {

  # Warning if both colorScheme and customColors are selected
  if (!is.null(colorScheme) && !is.null(customColors)) {
    warning("Both colorScheme and customColors are selected. Please choose only one.")
    return(GC_chart)
  }

  series <- GC_chart$x

  # Update legend
  GC_chart$x$legend$legendOptions$colorScheme <- colorScheme
  GC_chart$x$legend$legendOptions$customColors <- customColors

  # Capture clusters to update
  clusters <- names(GC_chart$x$series)

  # Update gene colors
  for(cluster_name in clusters){
    GC_chart$x$series[[cluster_name]]$genes$colorScheme <- colorScheme
    GC_chart$x$series[[cluster_name]]$genes$customColors <- customColors
  }

  return(GC_chart)
}

#' Set Legend for a Gene Chart
#'
#' This function configures the legend for a gene chart. It allows toggling the legend's visibility,
#' setting a background color, and assigning custom labels for the legend entries.
#' The function can also handle additional customizations through various options.
#'
#' @param GC_chart The gene chart object to be modified.
#' @param group Optional; character or NULL, specifies the groups to include in the legend.
#'   If NULL, groups are taken from the 'group' attribute of the 'GC_chart' object.
#' @param show Logical, specifies whether to display the legend.
#' @param backgroundColor String, the background color of the legend.
#' @param order Optional; list, specifies the order of the legend labels.
#' @param position Character. Position of the legend, either "top" or "bottom".
#' Default is "bottom".
#' @param style A list of CSS styles to be applied to the chart container. Each
#'   element of the list should be a valid CSS property-value pair. For example,
#'   list(backgroundColor = "white", border = "2px solid black").
#'   Default is an empty list.
#' @param legendOptions List, additional options for the legend.
#' @param legendTextOptions List, additional text options for the legend.
#' @param ... Additional arguments to be passed to the legend configuration.
#'
#' @return Returns the modified gene chart object with the legend configured.
#'
#' @examples
#' genes_data <- data.frame(
#'   start = c(10, 90, 130, 170, 210),
#'   end = c(40, 120, 160, 200, 240),
#'   name = c('Gene 1', 'Gene 3', 'Gene 4', 'Gene 5', 'Gene 6'),
#'   group = c('A', 'B', 'B', 'A', 'C'),
#'   cluster = c('A', 'A', 'A', 'B', 'B')
#' )
#'
#' # Customize legend
#' GC_chart(genes_data, cluster = "cluster", group = "group", height = "200px") %>%
#'   GC_legend(
#'     position = "top", #bottom
#'     orientation = "horizontal", #vertical
#'     x = 0,
#'     y = 0,
#'     width = NULL, # 100 / "100px" / 50%
#'     adjustHeight = TRUE,
#'     backgroundColor = "#0000",
#'     order = list(),
#'     positions = "bottom",
#'     style = list(
#'       backgroundColor = "#0000"
#'       # Any other CSS styles
#'     ),
#'     legendOptions = list(
#'       cursor = "pointer",
#'       colorScheme = NULL,
#'       customColors = NULL # c("red", "green", "orange")
#'       # Additional styles
#'     ),
#'     legendTextOptions = list(
#'       cursor = "pointer",
#'       textAnchor = "start",
#'       dy = ".35em",
#'       fontSize = "12px",
#'       fontFamily = "sans-serif",
#'       fill = "black"
#'       # Additional styles
#'     )
#'   )
#'
#' @export
GC_legend <- function(
    GC_chart,
    group = NULL,
    show = TRUE,
    backgroundColor = "#0000",
    order = list(),
    position = "bottom",
    style = list(),
    legendOptions = list(),
    legendTextOptions = list(),
    ...
) {

  if (!show) {
    return(GC_chart)
  }

  if (is.logical(group) && length(group) == 1) {
    show <- group
    group <- NULL
  }

  if (is.null(group) && is.null(GC_chart$x$group) && is.null(labels)){
    stop("Please define a group")
  }

  if (is.null(group) && !is.null(GC_chart$x$group)){
    group <- GC_chart$x$group
  }

  if (is.null(labels) && !(group %in% names(GC_chart$x$data))) {
    stop("group column not found in data")
  }

  # Capture arguments and filter out NULL or empty values
  options <- Filter(function(x) !is.null(x) && length(x) > 0, list(
    group = group,
    show = show,
    backgroundColor = backgroundColor,
    order = order,
    position = position,
    style = style,
    legendOptions = legendOptions,
    legendTextOptions = legendTextOptions,
    ...
  ))

  # Merge new options with existing options
  if (is.null(GC_chart$x$legend)) {
    GC_chart$x$legend <- options
  } else {
    GC_chart$x$legend <- modifyList(GC_chart$x$legend, options)
  }

  return(GC_chart)
}

#' Add Annotations to a GC_chart
#'
#' This function adds annotations to specified clusters within a GC chart.
#' Annotations can be of various types and are positioned based on provided coordinates.
#' The types of annotations available are: text, textMarker, line, arrow, symbol,
#' rectangle, promoter, and terminator.
#'
#' @param GC_chart A GC chart object to which the annotations will be added.
#' @param cluster Numeric or character vector specifying the clusters to which annotations should be added.
#' @param type Character vector specifying the type of annotations to add. The default is "text".
#' @param ... Additional parameters for customization of annotations, depending on the type.
#'
#' @return Updated GC chart object with added annotations.
#'
#' @examples
#' genes_data <- data.frame(
#'   start = c(10, 50, 90, 130, 170, 210),
#'   end = c(40, 80, 120, 160, 200, 240),
#'   name = c('Gene 1', 'Gene 2', 'Gene 3', 'Gene 4', 'Gene 5', 'Gene 6'),
#'   group = c('A', 'A', 'B', 'B', 'A', 'C'),
#'   cluster = c(1, 1, 1, 2, 2, 2)
#' )
#'
#' # Adding annotations to a GC chart
#' GC_chart(genes_data, cluster = "cluster", group = "group", height = "220px") %>%
#'   GC_annotation(
#'     type = "textMarker",
#'     cluster = 1,
#'     position = 24,
#'     text = "Gene 1",
#'     arrowSize = 8
#'   ) %>%
#'   GC_annotation(
#'     type = "text",
#'     text = "feature 1",
#'     x = 91,
#'     y = 71
#'   ) %>%
#'   GC_annotation(
#'     type = "symbol",
#'     symbol = "triangle",
#'     x = 95,
#'     y = 64,
#'     size = 10,
#'     rotation = 180
#'   ) %>%
#'   GC_annotation(
#'     type = "terminator",
#'     x = 81
#'   ) %>%
#'   GC_annotation(
#'     type = "promoter",
#'     x = 49
#'   ) %>%
#'   # Convenience function to track mouse position on hoover
#'   GC_trackMouse()
#'
#' @seealso \code{\link{GC_trackMouse}}
#'
#' @export
GC_annotation <- function(
    GC_chart,
    type = "textAnnotation",
    cluster = NULL,
    ...
) {

  # Update the GC_chart object with title and options for each cluster
  clusters <- getUpdatedClusters(GC_chart, cluster)

  for(i in seq_along(clusters)){

    # Capture arguments and filter out NULL or empty values
    options <- Filter(function(x) !is.null(x) && length(x) > 0, list(
      type = type,
      ...
    ))

    currentAnnotations <- GC_chart$x$series[[clusters[i]]]$annotations

    if(is.null(currentAnnotations) || length(currentAnnotations) == 0){
      GC_chart$x$series[[clusters[i]]]$annotations <- list(options)
    } else {
      GC_chart$x$series[[clusters[i]]]$annotations <- c(currentAnnotations, list(options))
    }

  }

  return(GC_chart)
}

#' Track Mouse Movement in a GC_chart
#'
#' This function enables or disables mouse tracking on specified clusters within
#' a GC chart. When enabled, the x and y coordinates of the mouse are displayed
#' which can be used to place annotations.
#'
#' @param GC_chart A GC chart object to which the annotations will be added.
#' @param show Logical, specifies whether to track the mouse or not.
#' @param cluster Numeric or character vector specifying the clusters to which
#' annotations should be added.
#' @return Updated GC chart object with mouse tracking settings.
#'
#' @examples
#' genes_data <- data.frame(
#'   start = c(10, 50, 90, 130, 170, 210),
#'   end = c(40, 80, 120, 160, 200, 240),
#'   name = c('Gene 1', 'Gene 2', 'Gene 3', 'Gene 4', 'Gene 5', 'Gene 6'),
#'   group = c('A', 'A', 'B', 'B', 'A', 'C'),
#'   cluster = c(1, 1, 1, 2, 2, 2)
#' )
#'
#' # Enable mouse tracking
#' GC_chart(genes_data, cluster = "cluster", group = "group", height = "220px") %>%
#' GC_trackMouse()
#'
#' @seealso \code{\link{GC_annotation}}
#'
#' @export
GC_trackMouse <- function(
    GC_chart,
    show = TRUE,
    cluster = NULL
) {

  # Update the GC_chart object with title and options for each cluster
  clusters <- getUpdatedClusters(GC_chart, cluster)

  for(i in seq_along(clusters)){

      GC_chart$x$series[[clusters[i]]]$trackMouse$show <- show

  }

  return(GC_chart)
}


#' Set Tooltip for a Gene Chart
#'
#' This function configures the tooltip for a gene chart.
#'
#' @param GC_chart The gene chart object to be modified.
#' @param formatter A character string defining the HTML content of the tooltip. It can
#'   include placeholders like `{start}` and `{end}` which will be replaced by actual
#'   data values. The default value shows start and end data.
#' @param show Logical, whether to display the tooltip or not.
#' @param cluster Optional; used to specify which clusters in the chart should have tooltips.
#' @param ... Additional arguments that can be used to further customize the tooltip.
#'
#' @return Returns the gene chart object with the tooltip configured.
#'
#' @examples
#' # Set tooltip
#' genes_data <- data.frame(
#'   start = c(10, 90, 130, 170, 210),
#'   end = c(40, 120, 160, 200, 240),
#'   name = c('Gene 1', 'Gene 2', 'Gene 3', 'Gene 4', 'Gene 5'),
#'   group = c('A', 'B', 'B', 'A', 'C')
#' )
#'
#' # Add tooltips to the gene chart
#' GC_chart(genes_data, group = "group", height = "200px") %>%
#' GC_tooltip(formatter = " <b>Start:</b> {start}<br><b>end:</b> {end}")
#'
#' @export
GC_tooltip <- function(
    GC_chart,
    formatter = "<b>Start:</b> {start}<br><b>End:</b> {end}",
    show = TRUE,
    cluster = NULL,
    ...
) {

  if (!show) {
    return(GC_chart)
  }

  if (is.logical(formatter) && length(formatter) == 1) {
    show <- formatter
  }

  # Capture ... arguments
  dots <- list(...)

  # Update the GC_chart object with title and options for each cluster
  clusters <- getUpdatedClusters(GC_chart, cluster)

  for(i in seq_along(clusters)){

    # Default options
    options <- list(
      formatter = formatter,
      show = show
    )

    # Add arguments to options
    for(name in names(dots)) {
      options[[name]] <- dots[[name]][(i-1) %% length(dots[[name]]) + 1]
    }

    # Set title options for each cluster
    GC_chart$x$series[[clusters[i]]]$tooltip <- options

  }
  return(GC_chart)
}

#' Modify Cluster Settings
#'
#' This function can switch prevention of gene overlap on, adjust the spacing
#' between tracks and alter the styling of specified clusters.
#'
#' @param GC_chart The gene chart object to be modified.
#' @param separate_strands Logical, indicating whether to vertically separate
#' forward and reverse genes.
#' @param strand_spacing Numeric, specifies the spacing between genes on
#' different strands. Used only if `separate_strands` is TRUE.
#' @param prevent_gene_overlap Logical, indicating whether to vertically separate
#' overlapping genes.
#' @param overlap_spacing Numeric, specifies the spacing between overlapping genes
#' Used only if `prevent_gene_overlap` is TRUE.
#' @param cluster Optional; used to specify which clusters in the chart should have tooltips.
#' @param style A list of CSS styles to be applied to the gene track.
#'              Each element of the list should be a valid CSS property-value
#'              pair. For example, list(backgroundColor = "red", color = "white").
#' @param ... Additional arguments to be passed to the underlying functions.
#'
#' @return Returns the modified gene chart object.
#'
#' @examples
#' genes_data <- data.frame(
#'   start = c(1, 10, 200, 220, 600),
#'   end = c(10, 150, 180, 400, 400),
#'   name = c('Gene 1', 'Gene 2', 'Gene 3', 'Gene 4', 'Gene 5'),
#'   group = c('A', 'A', 'A', 'A', 'A')
#' )
#'
#'
#' GC_chart(genes_data, group = "group", height = "150px") %>%
#'   GC_cluster(separate_strands=TRUE, strand_spacing = 0) %>%
#'   GC_legend(FALSE)
#'
#' @export
GC_cluster <- function(
    GC_chart,
    separate_strands = NULL,
    strand_spacing = NULL,
    prevent_gene_overlap = NULL,
    overlap_spacing=NULL,
    cluster = NULL,
    style = list(),
    ...
) {

  if (!is.null(separate_strands) && !is.null(prevent_gene_overlap) &&
      separate_strands == TRUE && prevent_gene_overlap == TRUE) {
    warning("Setting both 'separate_strands' and 'prevent_gene_overlap' to TRUE is not supported. Resetting to NULL.")
    separate_strands <- NULL
    prevent_gene_overlap <- NULL
  }

  # Update the GC_chart object with title and options for each cluster
  clusters <- getUpdatedClusters(GC_chart, cluster)


  for(i in seq_along(clusters)){

  # Add gene track
  if(!is.null(prevent_gene_overlap) && prevent_gene_overlap){
    subset_data <- GC_chart$x$series[[i]]$data
    subset_data <- subset_data[with(subset_data, order(-pmax(start, end), abs(end - start))), ]
    GC_chart$x$series[[i]]$data <- add_gene_track(subset_data)
  }

  # Update Style
  containerOptions <- Filter(function(x) !is.null(x) && length(x) > 0, list(
      style = style
    ))

  # Merge new options with existing options
  if (is.null(GC_chart$x$series[[clusters[i]]]$container$style)) {
    GC_chart$x$series[[clusters[i]]]$container$style <- containerOptions
  } else {
    GC_chart$x$series[[clusters[i]]]$container$style <-
      modifyList(GC_chart$x$series[[clusters[i]]]$container$style, containerOptions)
  }

  # Update cluster options
  clusterOptions <- Filter(function(x) !is.null(x) && length(x) > 0, list(
    separateStrands = separate_strands,
    strandSpacing = strand_spacing,
    subset_data = prevent_gene_overlap,
    overlapSpacing = overlap_spacing,
    ...
  ))

  # Merge new options with existing options
  if (is.null(GC_chart$x$series[[clusters[i]]]$cluster)) {
    GC_chart$x$series[[clusters[i]]]$cluster <- clusterOptions
  } else {
    GC_chart$x$series[[clusters[i]]]$cluster <-
      modifyList(GC_chart$x$series[[clusters[i]]]$cluster, clusterOptions)
  }

  }

  return(GC_chart)
}

#' Align gene clusters
#'
#' This function aligns clusters based on a specified gene id.
#'
#' @param GC_chart A chart object containing genomic data along with clustering
#' information.
#' @param id_column The name of the column that contains the gene identifiers.
#' @param id The specific identifier of the gene to be aligned.
#' @param align The alignment method for the gene. Valid values are
#' "left", "right", or "center". Defaults to "left".
#'
#' @return The modified `GC_chart` object with updated genomic coordinates.
#'
#' @examples
#' genes_data <- data.frame(
#'   start = c(10, 90, 130, 170, 210),
#'   end = c(40, 120, 160, 200, 240),
#'   name = c('Gene 1', 'Gene 2', 'Gene 3', 'Gene 4', 'Gene 5'),
#'   group = c('A', 'B', 'C', 'C', 'A'),
#'   cluster =  c(1, 1, 1, 2, 2)
#' )
#'
#'
#' GC_chart(genes_data, group = "group", cluster = "cluster", height = "150px") %>%
#'   GC_align("group", "A", align = "left") %>%
#'   GC_legend(FALSE)
#'
#' @export
GC_align <- function(
    GC_chart,
    id_column,
    id,
    align = "left") {
  if (is.null(GC_chart$x$cluster)) {
    warning("Could not align selected gene. Please define cluster in the GC_chart function.")
    return(GC_chart)
  }

  if (!(id_column %in% colnames(GC_chart$x$data))) {
    warning(paste("Column", id_column, "not found in the data."))
    return(GC_chart)
  }

  if (!(id %in% GC_chart$x$data[[id_column]])) {
    warning(paste("ID", id, "not found in the", id_column, "column."))
    return(GC_chart)
  }

  # Swap start and stop for reverse genes
  data <- GC_chart$x$data
  cluster_column <- GC_chart$x$cluster

  # Get reversed clusters
  clusters <- unique(data[[cluster_column]])
  reversed_clusters <- clusters[sapply(clusters, function(cl) {
    if (!is.null(GC_chart$x$series[[cl]]$scale) && !is.null(GC_chart$x$series[[cl]]$scale$reverse)) {
      return(GC_chart$x$series[[cl]]$scale$reverse == TRUE)
    } else {
      return(FALSE)
    }
  })]

  swapped_indices <- data$start > data$end
  data[swapped_indices, c("start", "end")] <- data[swapped_indices, c("end", "start")]

  adjusted_range <- adjust_range(data, cluster_column, id_column, id, align = align, reversed_clusters)
  update_clusters <- adjusted_range[[cluster_column]]

  for (cluster in update_clusters) {
    cluster_data <- adjusted_range[adjusted_range[[cluster_column]] == cluster, ]
    # Set scale options for each cluster
    GC_chart$x$series[[cluster]]$scale$start <- cluster_data$start
    GC_chart$x$series[[cluster]]$scale$end <- cluster_data$end
  }

  return(GC_chart)
}

#' Get Links from GC Chart
#'
#' Processes a `GC_chart` object to create links between clusters. It creates
#' links for all values in the group column or between value1 and value2 when
#' specified.
#'
#' @param GC_chart Gene chart object.
#' @param group The name of the column in the data to create value pairs from.
#' @param value1 Optional vector of group values to generate links for.
#' @param value2 Optional vector of group values to generate links for.
#' @param cluster Numeric or character vector or NULL; specifies which clusters
#' to generate links between.
#' @return A data frame of links between clusters based on the group column.
#' @examples
#' # See examples for the `GC_links` function for usage.
#' @seealso
#' * [GC_links()]
#' @noRd
get_links <-
  function(GC_chart,
           group,
           value1 = NULL,
           value2 = NULL,
           cluster = NULL) {

    data <- GC_chart$x$data

    # Check if group column exists in the data
    if (!(group %in% names(data))) {
      stop(paste("Column", group, "not found in chart data."))
      return(NULL)
    }

    # Remove rows with NA in the specified group column
    data <- data[!is.na(data[[group]]), ]
    data <- data[data[[group]] != "No Hit", ]

    # Check if all values in value1 and value2 are present in the group column
    if (!is.null(value1) && !all(value1 %in% data[[group]])) {
      stop("Some values in 'value1' are not present in the group column.")
      return(NULL)
    }
    if (!is.null(value2) && !all(value2 %in% data[[group]])) {
      stop("Some values in 'value2' are not present in the group column.")
      return(NULL)
    }

    if (!is.null(cluster) && length(cluster) < 2) {
      stop("At least two clusters need to be provided.")
      return(NULL)
    }
    clusters <- getUpdatedClusters(GC_chart, cluster)

    cluster_column <- GC_chart$x$cluster
    if (is.null(cluster_column)) {
      stop("Please define cluster in the GC_chart function.")
      return(NULL)
    }

    # Rename cluster column
    colnames(data)[colnames(data) == cluster_column] <- "cluster"

    cluster_pairs <-
      mapply(c, clusters[-length(clusters)], clusters[-1], SIMPLIFY = FALSE)
    data <- subset(data, data$cluster %in% clusters)

    all_merged_links <- NULL

    # Process each cluster pair
    for (pair in cluster_pairs) {
      cluster1 <- data[data$cluster == pair[1], ]
      cluster2 <- data[data$cluster == pair[2], ]

      if (is.null(value1) || is.null(value2)) {
        # Filter for common group IDs
        common_genes <- intersect(cluster1[[group]], cluster2[[group]])
        cluster1 <- cluster1[cluster1[[group]] %in% common_genes, ]
        cluster2 <- cluster2[cluster2[[group]] %in% common_genes, ]

        names(cluster1) <- paste0(names(cluster1), "1")
        names(cluster2) <- paste0(names(cluster2), "2")

        merged_links <- merge(cluster1, cluster2, by.x = paste0(group, "1"), by.y = paste0(group, "2"))

        all_merged_links <- rbind(all_merged_links, merged_links)
      } else {
        # Process each value pair
        if (!is.null(value1) &&
          !is.null(value2) &&
          length(value1) == length(value2)) {
          for (i in seq_along(value1)) {
            if (!(value1[i] %in% cluster1[[group]])) {
              temp <- value1[i]
              value1[i] <- value2[i]
              value2[i] <- temp
            }

            filtered_cluster1 <-
              cluster1[cluster1[[group]] == value1[i], ]
            filtered_cluster2 <-
              cluster2[cluster2[[group]] == value2[i], ]

            if (nrow(filtered_cluster1) == 0 ||
              nrow(filtered_cluster2) == 0) {
              next
            }

            names(filtered_cluster1) <- paste0(names(filtered_cluster1), "1")
            names(filtered_cluster2) <- paste0(names(filtered_cluster2), "2")
            filtered_cluster1$temp_key <- 1
            filtered_cluster2$temp_key <- 1

            merged_links <- merge(filtered_cluster1, filtered_cluster2, by.x = "temp_key", by.y = "temp_key")
            merged_links$temp_key <- NULL


            all_merged_links <-
              rbind(all_merged_links, merged_links)
          }
        }
      }
    }

    if (is.null(all_merged_links) || nrow(all_merged_links) == 0) {
      stop("No links found with the specified parameters.")
      return(NULL)
    }

    return(all_merged_links)
  }

#' Add Links to GC Chart
#'
#' Add links generated by `get_links` to a `GC_chart` object. Links are added
#' to the graph by their respective rowIDs.
#'
#' @param GC_chart Gene chart object.
#' @param group The name of the column in the data to create value pairs from.
#' @param data data.frame containing linking data.
#' @param value1 Optional vector of group values to generate links for.
#' @param value2 Optional vector of group values to generate links for.
#' @param cluster Numeric or character vector or NULL; specifies which clusters.
#' @param curve Logical; if `TRUE`, links are curved, otherwise straight.
#' @param measure Character; specifies which measure to use for link color intensity. Should be "identity", "similarity", or "none".
#' @param show_links Logical; if `TRUE`, links are shown, otherwise hidden.
#' @param label Logical; if `TRUE`, shows measure labels on the links, otherwise hidden.
#' @param normal_color Color for the links in their normal state.
#' @param inverted_color Color for inverted links.
#' @param use_group_colors Logical; if `TRUE`, color links by group.
#' @param color_bar Logical; if `TRUE`, the color bar is displayed.
#' @param colorBarOptions List of options to customize the color bar appearance.
#' @param linkStyle A list of CSS styles to apply to the links.
#' @param labelStyle A list of CSS styles specifically for the labels.
#' @param ... Additional arguments passed to the links.
#' @return Modified `GC_chart` object with added links.
#' @examples
#' # Add links between all groups in each cluster
#' genes_data <- data.frame(
#'   start = c(10, 90, 130, 170, 240, 250, 300, 340, 380, 420),
#'   end = c(40, 120, 160, 200, 210, 270, 330, 370, 410, 450),
#'   name = c('Gene 1', 'Gene 2', 'Gene 3', 'Gene 4', 'Gene 5',
#'            'Gene 6', 'Gene 7', 'Gene 8', 'Gene 9', 'Gene 10'),
#'   group = c('A', 'B', 'C', 'A', 'B', 'C', 'A', 'B', 'C', 'D'),
#'   identity = c(NA, NA, NA, 50, 40, 100, 60, 65, 20, NA),
#'   similarity = c(NA, NA, NA, 40, 30, 90, 50, 55, 10, NA),
#'   cluster = c(1, 1, 1, 2, 2, 2, 3, 3, 3, 3)
#' )
#' GC_chart(genes_data,
#'          cluster = "cluster",
#'          height = "200px") %>%
#'   GC_links(
#'     group = "group",
#'     value1 = "A",
#'     value2 = "B",
#'     measure = "identity",
#'     label = FALSE) %>%
#'   GC_labels(label = "group")
#'
#' # Add links between group A of cluster 1 and A and B of cluster 2
#' GC_chart(genes_data,
#'          cluster = "cluster",
#'          height = "200px") %>%
#'          GC_labels(label = "group") %>%
#'          GC_links(group = "group",
#'                    value1 = c("A", "A"),
#'                    value2 = c("B", "A"),
#'                    label = FALSE,
#'                    cluster = c(1,2))
#'
#' # Style links and color bar
#' GC_chart(genes_data,
#'          cluster = "cluster",
#'          height = "200px"
#' ) %>%
#'   GC_links(
#'     group = "group",
#'     data = NULL,
#'     curve = TRUE,
#'     measure = "identity",
#'     show_links = TRUE,
#'     label = TRUE,
#'     normal_color = "#1f77b4",
#'     inverted_color = "#d62728",
#'     use_group_colors = FALSE,
#'     color_bar = TRUE,
#'     colorBarOptions = list(
#'       x = 0,
#'       y = 24,
#'       width = 10,
#'       height = 60,
#'       labelOptions = list(
#'         fontSize = 8,
#'         xOffset = 2,
#'         yOffset = 0
#'         # Any other CSS style
#'       ),
#'       titleOptions = list(
#'         fontSize = 12,
#'         xOffset = 2,
#'         yOffset = 0
#'         # Any other CSS style
#'       ),
#'       barOptions = list(
#'         stroke = "#000",
#'         strokeWidth = 0.5,
#'         opacity = 1
#'         # Any other CSS style
#'       )
#'     ),
#'     linkStyle = list(
#'       stroke = "black",
#'       strokeWidth = 0.5,
#'       fillOpacity = 0.4
#'       # Any other CSS style
#'     ),
#'     labelStyle = list(
#'       fontSize = "8px"
#'       # Any other CSS style
#'     )
#'   ) %>%
#'   GC_labels(label = "group", cluster = 1) %>%
#'   GC_clusterLabel()
#' @seealso
#' * [get_links()]
#' @export
GC_links <- function(
    GC_chart,
    group = NULL,
    data = NULL,
    value1 = NULL,
    value2 = NULL,
    cluster = NULL,
    curve = TRUE,
    measure = "identity",
    show_links = TRUE,
    label = TRUE,
    normal_color = "#969696",
    inverted_color = "#d62728",
    use_group_colors = FALSE,
    color_bar = TRUE,
    colorBarOptions = list(),
    linkStyle = list(),
    labelStyle = list(),
    ...
) {

  # Validate measure
  if (!measure %in% c("identity", "similarity", "none")) {
    stop("Invalid measure specified. Choose 'identity', 'similarity', or 'none'.")
  }

  # Check for required data
  if (is.null(data) && is.null(GC_chart)) {
    stop("Error: Either 'group' or 'data' must be provided.")
  }

  # Retrieve links data or use the provided data
  if (!is.null(data)) {
    links_data <- data
    group <- NULL
    label <- FALSE
  } else {
    links_data <- get_links(GC_chart, group, value1 = value1, value2 = value2, cluster = cluster)
  }

  # Rename relevant columns
  if ("identity2" %in% names(links_data)) {
    names(links_data)[names(links_data) == "identity2"] <- "identity"
  }
  if ("similarity2" %in% names(links_data)) {
    names(links_data)[names(links_data) == "similarity2"] <- "similarity"
  }
  names(links_data)[names(links_data) == paste0(group, "1")] <- "group1"
  names(links_data)[names(links_data) == paste0(group, "2")] <- "group2"

  # Define relevant columns based on the measure
  link_columns <- c("cluster1", "group1", "start1", "end1",
                    "cluster2", "group2", "start2", "end2")
  if (measure != "none") {
    link_columns <- c(link_columns, measure)
  }

  if (use_group_colors) {
    group_color <- if (!is.null(GC_chart$x$group)) GC_chart$x$group else group
    links_data$groupColor <- links_data[[paste0(group_color, "2")]]
    link_columns <- c(link_columns, "groupColor")
    color_bar <- FALSE
  }

  links_data <- links_data[, link_columns[link_columns %in% names(links_data)], drop = FALSE]
  links_data <- links_data[order(links_data$start2), ]

  # Construct the links options with the new measure-related parameters
  links_options <- Filter(function(x) !is.null(x) && length(x) > 0, list(
    group = group,
    curve = curve,
    measure = measure,
    showLinks = show_links,
    label = label,
    normalColor = normal_color,
    invertedColor = inverted_color,
    useGroupColors = use_group_colors,
    value1 = value1,
    value2 = value2,
    colorBar = color_bar,
    colorBarOptions = colorBarOptions,
    linkStyle = linkStyle,
    labelStyle = labelStyle,
    ...
  ))

  # Integrate the new links into the GC_chart object
  currentLinks <- GC_chart$x$links

  if (is.null(currentLinks) || length(currentLinks) == 0) {
    links_data$linkID <- paste0("0-", (1:nrow(links_data)))

    links <- list(
      data = links_data,
      options = links_options
    )

    GC_chart$x$links <- list(links)
  } else {
    links_data$linkID <- paste0(length(currentLinks), "-", (1:nrow(links_data)))

    links <- list(
      data = links_data,
      options = links_options
    )

    GC_chart$x$links <- c(currentLinks, list(links))
  }

  return(GC_chart)
}
