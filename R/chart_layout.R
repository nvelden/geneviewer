#' Add Title to a GC_chart
#'
#' Modify the cluster title and subtitle of specified clusters within a GC chart and adjust
#' the display settings.
#'
#' @param GC_chart A GC chart object.
#' @param title Character vector. Titles to set for the clusters.
#' @param subtitle Character vector. Subtitles to set for the clusters.
#' @param show Logical. Whether to display the title. Default is TRUE.
#' @param height Character. Height for the title (e.g., "50px"). Default is
#'   NULL: the height of the theme, otherwise 50 pixels with a subtitle and 30
#'   without.
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

  if (!is.logical(show) || length(show) != 1) {
    stop("show must be a single TRUE or FALSE")
  }

  if (!show || is.null(title) && is.null(subtitle)) {
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
  if (!any(show)) {
    return(GC_chart)
  }

  # Capture ... arguments
  dots <- list(...)

  # Update the GC_chart object with title and options for each cluster
  clusters <- getUpdatedClusters(GC_chart, cluster)

  for (i in seq_along(clusters)) {
    # Default options
    options <- list(
      title = recycle_at(title, i),
      subtitle = recycle_at(subtitle, i),
      subtitleFont = subtitleFont,
      titleFont = titleFont,
      show = recycle_at(show, i)
    )

    # Add arguments to options
    for (name in names(dots)) {
      options[[name]] <- recycle_at(dots[[name]], i)
    }

    # Set height for each cluster
    GC_chart$x$series[[clusters[i]]]$grid$margin$top <- recycle_at(height, i)

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
#' @param cluster Numeric or character vector specifying clusters to update.
#' @param y Vertical position of the sequence line. Default is NULL: as set by
#'   the theme, otherwise 50.
#' @param sequenceStyle A list of styling options for the sequence line.
#' @param markerStyle A list of styling options for the sequence break markers, e.g. `markerHeight`, `tiltAmount` and `gap` (distance in pixels between each marker and the break), plus any CSS style.
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
    y = NULL,
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
      show = recycle_at(show, i),
      y = recycle_at(y, i),
      sequenceStyle = sequenceStyle,
      markerStyle = markerStyle
    )

    # Add ... arguments to options
    for(name in names(dots)) {
      options[[name]] <- recycle_at(dots[[name]], i)
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

  if(direction == "row"){
    GC_chart$x$graphContainer$direction <- "row"
    # compute the % width for each cluster
    num_clusters <- length(update_clusters)
    if (num_clusters > 0) {
      # Round to 3 decimals
      cluster_width_pct <- round(100 / num_clusters, 3)
      for (cluster_name in update_clusters) {
        # Convert numeric to character and append '%'
        pct_str <- paste0(cluster_width_pct, "%")
        GC_chart$x$series[[cluster_name]]$container$style$width <- pct_str
        GC_chart$x$series[[cluster_name]]$container$width <- pct_str
      }
    }
  }

  # Update margins
  if(!is.null(margin) && is.null(cluster)){

    paddingTop <- if (!is.null(margin[["top"]])) get_relative_height(chart_height, margin[["top"]]) else 0
    paddingBottom <- if (!is.null(margin[["bottom"]])) get_relative_height(chart_height, margin[["bottom"]]) else 0

    if (!is.numeric(paddingTop) || !is.numeric(paddingBottom)) {
      stop("Use pixel values for the top and bottom margins when the chart height is a percentage.")
    }

    margin[["top"]] <- NULL
    margin[["bottom"]] <- NULL

    # Update left and right margins
    GC_chart$x$legend$margin <- if (is.null(GC_chart$x$legend$margin)) margin else utils::modifyList(GC_chart$x$legend$margin, margin)
    GC_chart$x$title$margin <- if (is.null(GC_chart$x$title$margin)) margin else utils::modifyList(GC_chart$x$title$margin, margin)

    # Update top and bottom margins
    if(paddingTop > 0){
      GC_chart$x$style[["paddingTop"]] <- paste0(as.character(paddingTop), "px")
    }
    if(paddingBottom > 0){
      GC_chart$x$style[["paddingBottom"]] <- paste0(as.character(paddingBottom), "px")
    }

    # Take the margins from the cluster heights. Percentage heights are
    # reduced in the browser, once the height in pixels is known.
    if(paddingTop > 0 || paddingBottom > 0){
      GC_chart$height <- "100%"
      for (cluster_name in all_clusters) {
        container_height <- GC_chart$x$series[[cluster_name]]$container$height
        if (is.character(container_height) && grepl("%$", container_height)) {
          next
        }
        current_height <- get_relative_height(container_height, container_height)
        GC_chart$x$series[[cluster_name]]$container$height <-
          current_height - ((paddingTop + paddingBottom) / length(all_clusters))
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
      current_width <- recycle_at(width, i)
      current_width <- get_relative_height(GC_chart$width, current_width)
      GC_chart$x$series[[cluster_name]]$container$style$width <- current_width
      GC_chart$x$series[[cluster_name]]$container$width <- current_width
    }

    # Update height if provided
    if (!is.null(height)) {
      if(!is.null(cluster)){
      current_height <- recycle_at(height, i)
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
#' format of '2%'. Default is NULL: as set by the theme, otherwise 2.
#' @param hidden Logical flag indicating whether the axis is hidden. Default is
#' FALSE.
#' @param breaks List specifying settings for the scale breaks. Default is an
#' empty list ().
#' @param axis_position Character string indicating the type of the axis ('top' or
#' bottom'). Default is NULL: as set by the theme, otherwise 'bottom'.
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
#' Default is NULL: as set by the theme, otherwise 10.
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
                     padding = NULL,
                     hidden = FALSE,
                     breaks = list(),
                     tickValues = NULL,
                     reverse = FALSE,
                     axis_position = NULL,
                     axis_type = "position", # range
                     y = NULL,
                     scale_breaks = FALSE,
                     scale_break_threshold = 20,
                     scale_break_padding = 1,
                     ticksCount = NULL,
                     ticksFormat = NULL,
                     tickStyle = list(),
                     textStyle = list(),
                     lineStyle = list(),
                     ...) {
  # Capture ... arguments
  dots <- list(...)

  xMin <- NULL
  xMax <- NULL

  # Update the GC_chart object with title and options for each cluster
  clusters <- getUpdatedClusters(GC_chart, cluster)

  if(axis_type == "range"){
    if(is.null(ticksFormat)){
      ticksFormat <- ".2s"
    }
    # Shift the genes of each selected cluster to start at 1
    for (clust in clusters) {
      cluster_data <- GC_chart$x$series[[clust]]$data
      min_start_end <- min(cluster_data$start, cluster_data$end)
      cluster_data$start <- cluster_data$start - min_start_end + 1
      cluster_data$end <- cluster_data$end - min_start_end + 1
      GC_chart$x$series[[clust]]$data <- cluster_data
      rows <- match(cluster_data$rowID, GC_chart$x$data$rowID)
      GC_chart$x$data$start[rows] <- cluster_data$start
      GC_chart$x$data$end[rows] <- cluster_data$end
    }
    xMin <- 1
    xMax <- max(unlist(lapply(clusters, function(clust) {
      c(GC_chart$x$series[[clust]]$data$start, GC_chart$x$series[[clust]]$data$end)
    })))
  }

  for (i in seq_along(clusters)) {

    subset_data <- GC_chart$x$series[[clusters[i]]]$data

    # With a range axis, show one shared axis below the last cluster
    if (axis_type == "range" && !hidden && i < length(clusters)) {
      hidden_current <- TRUE
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
      start = recycle_at(start, i),
      end = recycle_at(end, i),
      xMin = xMin,
      xMax = xMax,
      padding = padding,
      hidden = hidden_current,
      breaks = breaks_data,
      tickValues = tickValues,
      reverse = recycle_at(reverse, i),
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

    # Add additional options
    for (name in names(dots)) {
      options[[name]] <- recycle_at(dots[[name]], i)
    }

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

  if (!any(show)) {
    return(GC_chart)
  }

  # Capture ... arguments
  dots <- list(...)

  # Update the GC_chart object with title and options for each cluster
  clusters <- getUpdatedClusters(GC_chart, cluster)

  for(i in seq_along(clusters)){

    # Default options
    options <- list(
      show = recycle_at(show, i),
      scaleBarLineStyle = scaleBarLineStyle,
      scaleBarTickStyle = scaleBarTickStyle,
      labelStyle = labelStyle
    )

    # Add ... arguments to options
    for(name in names(dots)) {
      options[[name]] <- recycle_at(dots[[name]], i)
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
#' @param width Character. The width of the cluster label, which sets the left
#' or right margin of the cluster. Default is NULL: a margin set with
#' [GC_grid()], otherwise the width of the theme, otherwise "100px".
#' @param cluster Numeric or character vector. Clusters in the GC chart to update.
#' Default is NULL.
#' @param position Character. Position of the label, either "left" or "right".
#' Default is NULL: the position of the theme, otherwise "left".
#' @param wrapLabel Logical. Indicates whether the label should be wrapped.
#' Default is NULL: as set by the theme, otherwise TRUE.
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
    width = NULL,
    cluster = NULL,
    position = NULL,
    wrapLabel = NULL,
    wrapOptions = list(),
    ...
) {

  if (!any(show)) {
    return(GC_chart)
  }

  cluster_column <- GC_chart$x$cluster

  # Capture ... arguments
  dots <- list(...)

  # Update the GC_chart object with title and options for each cluster
  clusters <- getUpdatedClusters(GC_chart, cluster)

  # By default, label each selected cluster with its own name
  if(!is.null(cluster_column) && is.null(title)){
    title <- clusters
  }

  for(i in seq_along(clusters)){

    # Default options. The browser sets the cluster margin for the label from
    # its width and position, so a theme can set them too (clusterLabelMargin())
    options <- Filter(Negate(is.null), list(
      title = recycle_at(title, i),
      show = recycle_at(show, i),
      width = recycle_at(width, i),
      wrapLabel = wrapLabel,
      wrapOptions = wrapOptions,
      position = recycle_at(position, i)
    ))

    # Add ... arguments to options
    for(name in names(dots)) {
      options[[name]] <- recycle_at(dots[[name]], i)
    }

    # Set clusterLabel options for each cluster
    GC_chart$x$series[[clusters[i]]]$clusterLabel <- options

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
#' GC_chart(genes_data, cluster = "cluster", group = "group") %>%
#'   GC_clusterFooter(
#'     title = "Cluster Footer",
#'     subtitle = "Cluster subtitle"
#'   )
#' # Add styling to the title and subtitle
#' GC_chart(genes_data, cluster = "cluster", group = "group") %>%
#'   GC_clusterFooter(
#'     title = "This is a footer",
#'     subtitle = "Subtitle for the footer",
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
    show = TRUE,
    cluster = NULL,
    subtitleFont = list(),
    titleFont = list(),
    ...
) {

  if (!any(show)) {
    return(GC_chart)
  }

  # Capture ... arguments
  dots <- list(...)

  # Update the GC_chart object with title and options for each cluster
  clusters <- getUpdatedClusters(GC_chart, cluster)

  for(i in seq_along(clusters)){

    # Default options
    options <- list(
      title = recycle_at(title, i),
      subtitle = recycle_at(subtitle, i),
      show = recycle_at(show, i),
      subtitleFont = subtitleFont,
      titleFont = titleFont
    )

    # Add ... arguments to options
    for(name in names(dots)) {
      options[[name]] <- recycle_at(dots[[name]], i)
    }

    # Set footer options for each cluster
    GC_chart$x$series[[clusters[i]]]$footer <- options
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
#' @param ticksFormat Format for tick labels. Default is ",.0f".
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
#'   ticksFormat = ",.0f", # ".2s",
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
    ticksFormat = NULL,
    tickStyle = list(),
    textStyle = list(),
    cluster = NULL,
    ...
) {

  if (!any(show)) {
    return(GC_chart)
  }

  # Capture ... arguments
  dots <- list(...)

  # Update the GC_chart object with title and options for each cluster
  clusters <- getUpdatedClusters(GC_chart, cluster)

  for(i in seq_along(clusters)){

    # Default options
    options <- list(
      show = recycle_at(show, i),
      ticksFormat = ticksFormat,
      tickStyle = tickStyle,
      textStyle = textStyle
    )

    # Add ... arguments to options
    for(name in names(dots)) {
      options[[name]] <- recycle_at(dots[[name]], i)
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
    subset_data <- GC_chart$x$series[[clusters[i]]]$data
    subset_data <- subset_data[with(subset_data, order(-pmax(start, end), abs(end - start))), ]
    GC_chart$x$series[[clusters[i]]]$data <- add_gene_track(subset_data)
  }

  # Update style options
  if (is.null(GC_chart$x$series[[clusters[i]]]$container$style)) {
    GC_chart$x$series[[clusters[i]]]$container$style <- style
  } else {
    GC_chart$x$series[[clusters[i]]]$container$style <-
      modifyList(GC_chart$x$series[[clusters[i]]]$container$style, style)
  }

  # Update cluster options
  clusterOptions <- Filter(function(x) !is.null(x) && length(x) > 0, list(
    separateStrands = separate_strands,
    strandSpacing = strand_spacing,
    preventGeneOverlap = prevent_gene_overlap,
    overlapSpacing = overlap_spacing,
    ...
  ))

  if (is.null(GC_chart$x$series[[clusters[i]]]$cluster)) {
    GC_chart$x$series[[clusters[i]]]$cluster <- clusterOptions
  } else {
    GC_chart$x$series[[clusters[i]]]$cluster <-
      modifyList(GC_chart$x$series[[clusters[i]]]$cluster, clusterOptions)
  }

  # Update container options
  containerOptions <- Filter(function(x) !is.null(x) && length(x) > 0, list(
    ...
  ))

  if (is.null(GC_chart$x$series[[clusters[i]]]$container)) {
    GC_chart$x$series[[clusters[i]]]$container <- containerOptions
  } else {
    GC_chart$x$series[[clusters[i]]]$container <-
      modifyList(GC_chart$x$series[[clusters[i]]]$container, containerOptions)
  }

  }

  return(GC_chart)
}
