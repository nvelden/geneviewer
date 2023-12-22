#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`

#' Create a GC Chart Visualization
#'
#' Generates an interactive GC chart for genomic data.
#'
#' @param data Data frame containing genomic information.
#' @param start Column name that indicates start positions. Default is "start".
#' @param end Column name that indicates end positions. Default is "end".
#' @param cluster Optional column name used for clustering purposes. Default is
#'   NULL.
#' @param group Column name used for gene grouping to influence color
#'   aesthetics.
#' @param width Width specification for the chart, such as '100\%' or 500.
#'   Default is unspecified.
#' @param height Height specification for the chart, such as '400px' or 300.
#'   Default is unspecified.
#' @param style A list of CSS styles to be applied to the chart container. Each
#'   element of the list should be a valid CSS property-value pair. For example,
#'   list(backgroundColor = "white", border = "2px solid black").
#'   Default is an empty list.
#' @param background_color Background color for the chart,
#' specified as a color code. Default is transparent.
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
#' GC_chart(genes_data, group = "group", cluster = "cluster", height = "200px") %>%
#' GC_labels("name")
#'
#' @import htmlwidgets
#' @export
GC_chart <- function(data, start = "start", end = "end", cluster = NULL, group = NULL, width = "100%", height = "400px", style = list(), elementId = NULL){

  # ensure that data is a data frame
  stopifnot(is.data.frame(data))

  # Check if column names are in the data frame
  colnames_data <- colnames(data)
  if (!(start %in% colnames_data)) stop("start column not found in data")
  if (!(end %in% colnames_data)) stop("end column not found in data")
  if (!is.null(cluster) && !(cluster %in% colnames_data)){
    stop("cluster column not found in data")
  }
  if (!is.null(group) && !(group %in% colnames_data)) {
    stop("group column not found in data")
  }

  x <- list()

  show_legend <- if(!is.null(group)) TRUE else FALSE

  # Add rowID to data
  data$rowID <- seq_len(nrow(data))

  x$data <- data
  x$group <- group
  x$style <- style
  x$title$style <- list(width = "100%")
  x$legend <- list(group = group, show = show_legend, position = "bottom",
                   width = width)
  x$legend$style <- list(width = "100%", backgroundColor = style$backgroundColor)


  if(is.null(cluster)){
    clusters <- "cluster"
  } else {
    clusters <- unique(as.character(data[[cluster]]))
  }

  for(clust in clusters){

    # Subset data for the current cluster
    if(is.null(cluster)){
      subset_data <- data
    } else {
      subset_data <- data[data[[cluster]] == clust, ]
    }

    subset_data$start <- subset_data[[start]]
    subset_data$end <- subset_data[[end]]

    subset_data <- subset_data[with(subset_data, order(-pmax(start, end), abs(end - start))), ]
    subset_data <- add_gene_track(subset_data)
    subset_data$cluster <- clust

    # Data
    x$series[[clust]]$clusterName <- clust
    x$series[[clust]]$data <- subset_data

    x$series[[clust]]$options <- list(height = compute_size(height, length(clusters)), width = width)
    x$series[[clust]]$options$style <- list(width = "100%", backgroundColor = style$backgroundColor)
    x$series[[clust]]$genes <- list(group = group, show = TRUE)
    x$series[[clust]]$labels <- list(group = group, show = TRUE)
    x$series[[clust]]$coordinates <- list(show = FALSE)
    x$series[[clust]]$scaleBar <- list()
    x$series[[clust]]$footer <- list()
    x$series[[clust]]$clusterLabel <- list()
    x$series[[clust]]$clusterTitle <- list()
    x$series[[clust]]$sequence <- list(show = TRUE)
    x$series[[clust]]$tooltip <- list(show = TRUE, formatter ="<b>Start:</b> {start} <br><b>end:</b> {end}")
  }

  # create the widget
  htmlwidgets::createWidget(
    name = 'GCVieweR',
    x,
    width = width,
    height = height,
    package = 'GCVieweR',
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
#' GC_chart(genes_data, cluster = "cluster", group = "group", height = "400px") %>%
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
    height = "50px",
    ...
) {

  if (!show) {
    return(GC_chart)
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
#'     y = 0,
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
      title = title[(i-1) %% length(title) + 1],
      subtitle = subtitle[(i-1) %% length(subtitle) + 1],
      subtitleFont = subtitleFont,
      titleFont = titleFont,
      show = show
    )

    # Add arguments to options
    for(name in names(dots)) {
      options[[name]] <- dots[[name]][(i-1) %% length(dots[[name]]) + 1]
    }

    # Set height for each cluster
    GC_chart$x$series[[clusters[i]]]$grid$margin$top <- height[(i-1) %% length(height) + 1]

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
    cluster = NULL
) {

  all_clusters <- names(GC_chart$x$series)
  update_clusters <- getUpdatedClusters(GC_chart, cluster)
  chart_height <- GC_chart$height

  for (i in seq_along(update_clusters)) {
    cluster_name <- update_clusters[i]

    # Update margins if provided
    if (!is.null(margin)) {
        default_margin <- GC_chart$x$series[[cluster_name]]$grid$margin
        GC_chart$x$series[[cluster_name]]$grid$margin <- if (is.null(default_margin)) margin else utils::modifyList(default_margin, margin)
    }

    # Update width if provided
    if (!is.null(width)) {
      current_width <- width[(i-1) %% length(width) + 1]
      if (is.numeric(current_width)) {
        current_width <- paste0(current_width, "%")
      }
      GC_chart$x$series[[cluster_name]]$grid$width <- current_width
    }

    # Update height if provided
    if (!is.null(height)) {
      if(!is.null(cluster)){
      current_height <- height[(i-1) %% length(height) + 1]
      current_height <- get_relative_height(chart_height, current_height)
      GC_chart$x$series[[cluster_name]]$grid$height <- current_height
      } else {
        GC_chart$x$series[[cluster_name]]$grid$height <- compute_size(height, length(all_clusters))
      }
    }
  }

  # Update Title and Legend margins
  if(!is.null(margin) && is.null(cluster)){
  GC_chart$x$legend$margin <- if (is.null(default_margin)) margin else utils::modifyList(default_margin, margin)
  GC_chart$x$title$margin <- if (is.null(default_margin)) margin else utils::modifyList(default_margin, margin)
  }
  # Update total height of chart
  total_height <- 0

  for(cluster_name in all_clusters) {
    cluster_height <- GC_chart$x$series[[cluster_name]]$grid$height
    total_height <- total_height + get_relative_height(GC_chart$height, cluster_height)
  }

  GC_chart$height <- total_height

  return(GC_chart)
}

#' Update Scale of a GC Chart Cluster
#'
#' Modify the scale settings for specified clusters within a GC chart.
#'
#' @param GC_chart A GC chart object.
#' @param cluster Numeric or character vector specifying clusters in the GC
#'   chart to update.
#' @param start Numeric vector indicating the starting point for the scale.
#'   Default is NULL.
#' @param end Numeric vector indicating the end point for the scale.
#'   Default is NULL.
#' @param hidden Logical flag indicating whether the axis is hidden. Default is
#'   FALSE.
#' @param breaks List specifying settings for the scale breaks. Default is an
#'   empty list ().
#' @param axisType Character string indicating the type of the axis ('top' or
#'   'bottom'). Default is 'bottom'.
#' @param tickValues Numeric vector or NULL, custom tick values to be used at
#'   the top of the cluster. If NULL, the default tick values are used.
#' @param reverse Logical flag indicating whether to reverse the scale for the
#'   corresponding cluster. Default is FALSE.
#' @param scale_breaks Logical flag indicating if scale breaks should be
#'   employed. Default is FALSE.
#' @param scale_break_threshold Numeric value indicating the threshold
#'   percentage for determining scale breaks. Default is 20.
#' @param scale_break_padding Numeric value indicating the padding on either
#'   side of a scale break. Default is 1.
#' @param ticksCount Numeric value indicating the number of ticks on the scale.
#'   Default is 20.
#' @param ticksFormat Character string indicating the format of the ticks.
#'   Default is ",.0f".
#' @param y Numeric value from 1 to 100 indicating the y-position of the x-axis.
#'   Default is NULL.
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
#'     hidden = FALSE,
#'     reverse = FALSE,
#'     axisType = "bottom",
#'     # breaks = list(
#'     #  list(start = 160, end = 900),
#'     #  list(start = 1600, end = 1900)
#'     # ),
#'     # tickValues = c(1, 2600),
#'     scale_breaks = TRUE,
#'     scale_break_threshold = 20,
#'     scale_break_padding = 1,
#'     ticksCount = 20,
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
                     hidden = FALSE,
                     breaks = list(),
                     tickValues = NULL,
                     reverse = FALSE,
                     axisType = "bottom",
                     y = NULL,
                     scale_breaks = FALSE,
                     scale_break_threshold = 20,
                     scale_break_padding = 1,
                     ticksCount = 20,
                     ticksFormat = ",.0f",
                     tickStyle = list(),
                     textStyle = list(),
                     lineStyle = list(),
                     ...) {
  # Capture ... arguments
  dots <- list(...)

  # Update the GC_chart object with title and options for each cluster
  clusters <- getUpdatedClusters(GC_chart, cluster)

  for (i in seq_along(clusters)) {
    # Calculate the index for each parameter considering their different lengths
    start_idx <- (i - 1) %% length(start) + 1
    stop_idx <- (i - 1) %% length(end) + 1
    reverse_idx <- (i - 1) %% length(reverse) + 1

    # Subset data for the current cluster
    subset_data <- GC_chart$x$series[[clusters[i]]]$data

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
      hidden = hidden,
      breaks = breaks_data,
      tickValues = tickValues,
      reverse = reverse[reverse_idx],
      axisType = axisType,
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
#' GC_chart(genes_data, cluster = "cluster", group = "group", height = "200px") %>%
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

  # Capture ... arguments
  dots <- list(...)

  # Update the GC_chart object with title and options for each cluster
  clusters <- getUpdatedClusters(GC_chart, cluster)

  for(i in seq_along(clusters)){


    # Default options
    options <- list(
      label = label[(i-1) %% length(label) + 1],
      show = show[(i-1) %% length(show) + 1],
      itemStyle = itemStyle
    )

    # Add ... arguments to options
    for(name in names(dots)) {
      options[[name]] <- dots[[name]][(i-1) %% length(dots[[name]]) + 1]
    }
    # Set labels options for each cluster
    GC_chart$x$series[[clusters[i]]]$labels <- options

  }

  # Check if GC_track is called last
  if (!is.null(GC_chart$x$track_called) && GC_chart$x$track_called) {
    warning("GC_track must be called after setting genes, labels or coordinates for proper effect.")
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
#' @param cluster Numeric or character, specifies the clusters to be affected by
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

  # Check if GC_track is called last
  if (!is.null(GC_chart$x$track_called) && GC_chart$x$track_called) {
    warning("GC_track must be called after setting genes, labels or coordinates for proper effect.")
  }

  return(GC_chart)
}


#' Modify Gene Characteristics within a Chart
#'
#' This function updates a gene chart with specific characteristics for genes based on the given parameters.
#' It can show/hide genes, apply a color scheme, assign custom colors, filter by cluster, and accept additional options.
#'
#' @param GC_chart The gene chart object to be modified.
#' @param group Character or NULL, groups to show or hide in the chart. If NULL, the group is taken from the chart object.
#' @param show Logical, whether to show the genes or not.
#' @param colorScheme Character or NULL, the name of the color scheme to use.
#' @param customColors List or NULL, custom colors to apply to the genes.
#' @param cluster Numeric or character, the specific cluster to filter genes by.
#' @param itemStyle List, a list of styles to apply to individual items in the chart.
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
#'     colorScheme = NULL, # One of D3.js build in colorSchemes
#'                         # (eg. "schemeCategory10",
#'                         # "schemeAccent", "schemeTableau10")
#'     customColors = NULL, # A vector of color names
#'     prevent_overlap = FALSE,
#'     track_spacing = 40,
#'     cluster = 1, # Specify a specific cluster
#'     x = 1,
#'     y = 50,
#'     stroke = "black",
#'     strokeWidth = 1,
#'     arrowheadWidth = 10,
#'     arrowheadHeight = 20,
#'     arrowHeight = 10
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
    show = TRUE,
    colorScheme = NULL,
    customColors = NULL,
    cluster = NULL,
    itemStyle = list(),
    ...
) {

  if (!show) {
    return(GC_chart)
  }

  if (is.logical(group) && length(group) == 1) {
    show <- group
    group <- NULL
  }

  if (is.null(group) && is.null(GC_chart$x$group)){
    stop("Please define a group")
  }

  if (is.null(group) && !is.null(GC_chart$x$group)){
    group <- GC_chart$x$group
  }

  if (!(group %in% names(GC_chart$x$data))) {
    stop("group column not found in data")
  }

  # Update the GC_chart object with title and options for each cluster
  clusters <- getUpdatedClusters(GC_chart, cluster)

  for(i in seq_along(clusters)){

    # Default options
    options <- Filter(function(x) !is.null(x) && length(x) > 0, list(
      group = group[(i-1) %% length(group) + 1],
      show = show[(i-1) %% length(show) + 1],
      colorScheme = colorScheme,
      customColors = customColors,
      itemStyle = itemStyle,
      ...
    ))

    GC_chart$x$series[[clusters[i]]]$genes <- options

  }

  # Check if GC_track is called last
  if (!is.null(GC_chart$x$track_called) && GC_chart$x$track_called) {
    warning("GC_track must be called after setting genes, labels or coordinates for proper effect.")
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
    formatter = "<b>Start:</b> {start}<br><b>end:</b> {end}",
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

#' Modify Gene Track
#'
#' This function can switch gene tracks off or adjust the spacing between tracks.
#'
#' @param GC_chart The gene chart object to be modified.
#' @param track Logical, whether to include the gene track or not.
#'              If FALSE, the specified gene track is removed.
#' @param spacing Numeric, the spacing to be used between gene tracks.
#' @param cluster Numeric or character, specifies the cluster to filter genes by.
#' @param style A list of CSS styles to be applied to the gene track.
#'              Each element of the list should be a valid CSS property-value
#'              pair. For example, list(backgroundColor = "red", color = "white").
#' @param ... Additional arguments to be passed to the underlying functions.
#'
#' @return Returns the modified gene chart object.
#'
#' @examples
#' genes_data <- data.frame(
#'   start = c(10, 20, 30, 170, 210),
#'   end = c(200, 150, 180, 200, 240),
#'   name = c('Gene 1', 'Gene 2', 'Gene 3', 'Gene 4', 'Gene 5'),
#'   group = c('A', 'A', 'A', 'A', 'A')
#' )
#'
#'
#' GC_chart(genes_data, group = "group", height = "150px") %>%
#'   GC_track(spacing=30) %>%
#'   GC_legend(FALSE)
#'
#' @export
GC_cluster <- function(
    GC_chart,
    track = TRUE,
    spacing = 40,
    cluster = NULL,
    style = list(),
    ...
) {
  # Update the GC_chart object with title and options for each cluster
  clusters <- getUpdatedClusters(GC_chart, cluster)

  for(i in seq_along(clusters)){

    # Update Track
    if(!track){
    subset_data <- GC_chart$x$series[[i]]$data
      if("geneTrack" %in% names(subset_data)) {
        subset_data <- subset_data[, !(names(subset_data) %in% "geneTrack")]
        GC_chart$x$series[[i]]$data <- subset_data
      }
    }

    GC_chart$x$series[[clusters[i]]]$genes$trackSpacing <- spacing
    GC_chart$x$series[[clusters[i]]]$labels$trackSpacing <- spacing
    GC_chart$x$series[[clusters[i]]]$coordinates$trackSpacing <- spacing

    # Update Style
    options <- Filter(function(x) !is.null(x) && length(x) > 0, list(
      style = style,
      ...
    ))

    # Merge new options with existing options
    if (is.null(GC_chart$x$series[[clusters[i]]]$options)) {
      GC_chart$x$series[[clusters[i]]]$options <- options
    } else {
      GC_chart$x$series[[clusters[i]]]$options <-
        modifyList(GC_chart$x$series[[clusters[i]]]$options, options)
    }

  }

  # Add flag that function has been called.
  GC_chart$x$track_called <- TRUE

  return(GC_chart)
}
