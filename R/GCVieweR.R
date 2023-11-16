#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`

#' Create a GC Chart Visualization
#'
#' Generates an interactive GC chart for genomic data.
#'
#' @param data Data frame containing genomic information.
#' @param start Column name that indicates start positions. Default is "start".
#' @param stop Column name that indicates stop positions. Default is "stop".
#' @param cluster Optional column name used for clustering purposes. Default is NULL.
#' @param group Column name used for gene grouping to influence color aesthetics.
#' @param width Width specification for the chart, such as '100\%' or 500. Default is unspecified.
#' @param height Height specification for the chart, such as '400px' or 300. Default is unspecified.
#' @param elementId Optional identifier string for the widget. Default is NULL.
#' @param scale_breaks Logical flag indicating if scale breaks should be employed. Default is FALSE.
#' @param scale_break_threshold Numeric value indicating the threshold percentage of the entire range for determining inter-gene regions suitable for scale breaks. Default is 20.
#' @param scale_break_padding Numeric value indicating the padding percentage of the entire range on either side of a scale break. Default is 1.
#'
#' @return A GC chart widget.
#'
#' @examples
#' genes_data <- data.frame(
#'   start = c(10, 50, 90, 130, 170, 210),
#'   stop = c(40, 80, 120, 160, 200, 240),
#'   name = c('Gene 1', 'Gene 2', 'Gene 3', 'Gene 4', 'Gene 5', 'Gene 6'),
#'   group = c('A', 'A', 'B', 'B', 'A', 'C'),
#'   cluster = c(1, 1, 1, 2, 2, 2)
#' )
#' GC_chart(genes_data, group = "group", cluster = "cluster") %>% GC_labels("name")
#'
#' @import htmlwidgets
#' @export
GC_chart <- function(data, start = "start", stop = "stop", cluster = NULL, group = NULL, width = "100%", height = "400px", background_color = "#0000", elementId = NULL, scale_breaks = FALSE, scale_break_threshold = 20, scale_break_padding = 1){

  # ensure that data is a data frame
  stopifnot(is.data.frame(data))

  # Check if column names are in the data frame
  colnames_data <- colnames(data)
  if (!(start %in% colnames_data)) stop("start column not found in data")
  if (!(stop %in% colnames_data)) stop("stop column not found in data")
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
  x$backgroundColor <- background_color
  x$legend <- list(group = group, show = show_legend, position = "bottom", backgroundColor = background_color)


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
    subset_data$stop <- subset_data[[stop]]
    subset_data <- subset_data[with(subset_data, order(-pmax(start, stop), abs(stop - start))), ]
    subset_data$cluster <- clust

    # Data
    x$series[[clust]]$clusterName <- clust
    x$series[[clust]]$data <- subset_data

    # Settings
    if(scale_breaks) {
      breaks_data <- get_scale_breaks(subset_data, threshold_percentage = scale_break_threshold, padding = scale_break_padding)
      x$series[[clust]]$scale <- list(breaks = breaks_data)
    }

    x$series[[clust]]$grid <- list(margin = list(left = "50px", right = "50px", top = 0, bottom = 0), height = compute_size(height, length(clusters)), width = width)
    x$series[[clust]]$style <- list(backgroundColor = background_color)
    x$series[[clust]]$title <- list()
    x$series[[clust]]$genes <- list(group = group, show = TRUE)
    x$series[[clust]]$labels <- list(group = group, show = TRUE)
    x$series[[clust]]$cluster <- list()
    x$series[[clust]]$coordinates <- list(show = FALSE)
    x$series[[clust]]$scaleBar <- list()
    x$series[[clust]]$footer <- list()
    x$series[[clust]]$clusterLabel <- list()
    x$series[[clust]]$sequence <- list(show = TRUE)
    x$series[[clust]]$tooltip <- list(show = TRUE, formatter ="<b>Start:</b> {start} <br><b>Stop:</b> {stop}")
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

#' Update Settings of a GC Chart Item
#'
#' This function modifies the settings of specified clusters within a GC chart.
#'
#' @param GC_chart A GC chart object to be updated.
#' @param setting Character. The setting within the cluster to be modified.
#' @param cluster Numeric or character vector. Cluster(s) within the GC chart to be updated.
#' @param ... Additional arguments to modify the settings of the cluster.
#'
#' @return An updated GC chart object.
#'
#' @examples
#' genes_data <- data.frame(
#'   start = c(10, 50, 90, 130, 170, 210),
#'   stop = c(40, 80, 120, 160, 200, 240),
#'   name = c('Gene 1', 'Gene 2', 'Gene 3', 'Gene 4', 'Gene 5', 'Gene 6'),
#'   group = c('A', 'A', 'B', 'B', 'A', 'C'),
#'   cluster = c(1, 1, 1, 2, 2, 2)
#' )
#'
#' # Update style of a specific cluster
#' GC_chart(genes_data, cluster ="cluster", group = "group") %>%
#' GC_labels("name") %>%
#' GC_item(
#'   "labels",
#'   cluster = 2,
#'   fontSize = "16px"
#' )
#'
#' # Update style of specific items within a specific cluster
#' GC_chart(genes_data, cluster ="cluster", group = "group") %>%
#' GC_labels("name") %>%
#' GC_item(
#'   "labels",
#'   cluster = 2,
#'   itemStyle = list(
#'     list(index = 0, fontSize = "10px"),  # Index 0 due to JS array
#'     list(index = 2, fontSize = "16px")
#'   )
#' )
#' @export
GC_item <- function(
    GC_chart,
    setting = NULL,
    cluster = NULL,
    ...
) {

  # Capture ... arguments
  dots <- list(...)

  # If cluster is NULL, retrieve all clusters from the GC_chart
  if (is.null(cluster)) {
    cluster <- unique(names(GC_chart$x$series))
  }

  # Update the GC_chart object with options for each cluster
  clusters <- getUpdatedClusters(GC_chart, cluster)

  for(clust in clusters){

    settings <- GC_chart$x$series[[clust]][[setting]]
    updated_settings <- modifyList(settings, dots)

    # Set options for each cluster
    GC_chart$x$series[[clust]][[setting]] <- updated_settings

  }

  return(GC_chart)
}

#' Update Title of a GC Chart Cluster
#'
#' Modify the title and subtitle of specified clusters within a GC chart and adjust
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
#' @param x Numeric. X-coordinate for title's position.
#' @param y Numeric. Y-coordinate for title's position.
#' @param position Character. Title position ("center", "left", or "right").
#' @param spacing Numeric. Spacing between title and subtitle.
#' @param ... Additional customization arguments for title and subtitle.
#'
#' @return Updated GC chart with new title settings.
#'
#' @examples
#' genes_data <- data.frame(
#'   start = c(10, 50, 90, 130, 170, 210),
#'   stop = c(40, 80, 120, 160, 200, 240),
#'   name = c('Gene 1', 'Gene 2', 'Gene 3', 'Gene 4', 'Gene 5', 'Gene 6'),
#'   group = c('A', 'A', 'B', 'B', 'A', 'C'),
#'   cluster = c(1, 1, 1, 2, 2, 2)
#' )
#'
#' # Basic usage
#' GC_chart(genes_data, cluster ="cluster", group = "group") %>%
#' GC_labels("name") %>%
#' GC_title(
#'   title = "Cluster 1 Data",
#'   subtitle = "Detailed View",
#'   show = TRUE,
#'   cluster = 1
#' )
#'
#' # Customizing title style
#' GC_chart(genes_data, cluster ="cluster", group = "group") %>%
#'   GC_labels("name") %>%
#'   GC_title(
#'     title = "Cluster 1 Data",
#'     subtitle = "Detailed View",
#'     show = TRUE,
#'     cluster = 1,
#'     x = 0,
#'     y = 0,
#'     position = "center",
#'     spacing = 20,
#'     titleFont = list(
#'       size = "16px",
#'       style = "normal",
#'       weight = "bold",
#'       decoration = "normal",
#'       family = "sans-serif",
#'       cursor = "default",
#'       fill = "black"
#'     ),
#'     subtitleFont = list(
#'       size = "14px",
#'       style = "normal",
#'       weight = "bold",
#'       decoration = "normal",
#'       family = "sans-serif",
#'       cursor = "default",
#'       fill = "black"
#'     )
#'   )
#'
#' @export
GC_title <- function(
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
    GC_chart$x$series[[clusters[i]]]$title <- options

  }
  return(GC_chart)
}

#' Update Sequence Display of a GC Chart Cluster
#'
#' Modify the sequence display of specified clusters within a GC chart and adjust
#' the display settings.
#'
#' @param GC_chart A GC chart object.
#' @param show Logical. Whether to display the sequence. Default is TRUE.
#' @param cluster Numeric or character vector. Clusters in the GC chart to update.
#' @param marker List. Settings for the sequence break marker.
#' @param ... Additional customization arguments for sequence display.
#'
#' @return Updated GC chart with new sequence display settings.
#'
#' @examples
#' genes_data <- data.frame(
#'   start = c(10, 90, 130, 170, 210),
#'   stop = c(40, 120, 160, 200, 240),
#'   name = c('Gene 1', 'Gene 3', 'Gene 4', 'Gene 5', 'Gene 6'),
#'   group = c('A', 'B', 'B', 'A', 'C'),
#'   cluster = c(1, 1, 2, 2, 2)
#' )
#'
#' # Basic usage
#' GC_chart(genes_data, cluster ="cluster", group = "group") %>%
#' GC_labels("name") %>%
#' GC_sequence(
#'   show = TRUE,
#'   cluster = 1
#' )
#'
#' # Customize sequence style
#' GC_chart(genes_data, cluster="cluster", group = "group") %>%
#' GC_sequence(
#'   stroke = "black",
#'   strokeWidth = 2,
#'   # Any other CSS styles
#'   marker =         # Style sequence
#'   list(            # break marker
#'     stroke = "black",
#'     strokeWidth = 2,
#'     gap = 3,
#'     tiltAmount = 5
#'     )
#'   )
#'
#' @export
GC_sequence <- function(
    GC_chart,
    show = TRUE,
    cluster = NULL,
    marker = list(),
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
      marker = marker
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
#'
#' @examples
#' genes_data <- data.frame(
#'   start = c(10, 90, 130, 170, 210),
#'   stop = c(40, 120, 160, 200, 240),
#'   name = c('Gene 1', 'Gene 3', 'Gene 4', 'Gene 5', 'Gene 6'),
#'   group = c('A', 'B', 'B', 'A', 'C'),
#'   cluster = c(1, 1, 2, 2, 2)
#' )
#'
#' # Set Margin of clusters
#' GC_chart(genes_data, cluster ="cluster", group = "group") %>%
#' GC_grid(margin = list(left = "10px", right = "10px"))
#'
#' # Set height of a specific cluster
#' GC_chart(genes_data, cluster ="cluster", group = "group") %>%
#' GC_grid(height = "200px", cluster = 2)
#'
#' @export
GC_grid <- function(
    GC_chart,
    margin = NULL,
    width = NULL,
    height = NULL,
    cluster = NULL
) {


  clusters <- getUpdatedClusters(GC_chart, cluster)

  for (i in seq_along(clusters)) {
    cluster_name <- clusters[i]

    # Update margins if provided
    if (!is.null(margin)) {
      GC_chart$x$series[[cluster_name]]$grid$margin <- modifyList(GC_chart$x$series[[cluster_name]]$grid$margin, margin)
    }

    # Update width if provided
    if (!is.null(width)) {
      current_width <- width[(i-1) %% length(width) + 1]
      # Convert numeric width to percentage string
      if (is.numeric(current_width)) {
        current_width <- paste0(current_width, "%")
      }
      GC_chart$x$series[[cluster_name]]$grid$width <- width
    }

    # Update height if provided
    if (!is.null(height)) {
      current_height <- height[(i-1) %% length(height) + 1]
      # Convert numeric height to percentage string
      GC_chart$x$series[[cluster_name]]$grid$height <- current_height
    }
  }

  return(GC_chart)
}

#' Update Scale of a GC Chart Cluster
#'
#' Modify the scale settings for specified clusters within a GC chart.
#'
#' @param GC_chart A GC chart object.
#' @param cluster Numeric or character vector. Clusters in the GC chart to update.
#' @param start Numeric vector. Starting points for the scales.
#' @param stop Numeric vector. Stopping points for the scales.
#' @param breaks List. Settings for the scale breaks.
#' @param ... Additional arguments for scale settings.
#'
#' @return Updated GC chart with new scale settings.
#'
#' @examples
#' genes_data <- data.frame(
#' start = c(100, 1000, 2000),
#' stop = c(150, 1500, 2500),
#' name = c( 'Gene 4', 'Gene 5', 'Gene 6'),
#' group = c( 'B', 'A', 'C'),
#' cluster = c(2, 2, 2)
#' )
#'
#' # Set manual start and stop values for scales domain
#' GC_chart(genes_data, cluster ="cluster", group = "group") %>%
#' GC_scale(
#'   start = 1,
#'   stop = 2600,
#'   breaks =
#'   list(
#'     list(start = 160, stop = 900),
#'     list(start = 1600, stop = 1900)
#'       )
#'   )
#'
#' @export
GC_scale <- function(
    GC_chart,
    cluster = NULL,
    start = NULL,
    stop = NULL,
    breaks = list(),
    ...
) {

  # Capture ... arguments
  dots <- list(...)

  # Update the GC_chart object with title and options for each cluster
  clusters <- getUpdatedClusters(GC_chart, cluster)

  for(i in seq_along(clusters)){

    # Default options
    options <- list()

    # Default options
    options <- list(
      start = start[(i-1) %% length(start) + 1],
      stop = stop[(i-1) %% length(stop) + 1],
      breaks = breaks
    )

    # Set scaleBar options for each cluster
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
#' @param ... Additional arguments for scale bar settings.
#'
#' @return Updated GC chart with new scale bar settings.
#'
#' @examples
#' genes_data <- data.frame(
#'  start = c(1000, 9000, 13000, 17000, 21000),
#'  stop = c(4000, 12000, 16000, 20000, 24000),
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
#' GC_chart(genes_data, cluster ="cluster", group = "group") %>%
#'   GC_scaleBar(
#'     title = " 1kb",
#'     scaleBarUnit = 1000,
#'     y = 50,
#'     labelPosition =  "left",
#'     fontSize =  "10px",
#'     fontFamily = "sans-serif",
#'     textPadding =  -2,
#'     scaleBarLine =
#'       list(
#'         stroke = "black",
#'         strokeWidth = 2
#'            ),
#'     scaleBarTick =
#'       list(
#'         stroke = "black",
#'         strokeWidth = 2
#'         )
#'        )
#'
#' @export
GC_scaleBar <- function(
    GC_chart,
    show = TRUE,
    cluster = NULL,
    scaleBarTick = list(),
    scaleBarLine = list(),
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
      scaleBarTick = scaleBarTick,
      scaleBarLine = scaleBarLine
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
#'   stop = c(40, 120, 160, 200, 240),
#'   name = c('Gene 1', 'Gene 3', 'Gene 4', 'Gene 5', 'Gene 6'),
#'   group = c('A', 'B', 'B', 'A', 'C'),
#'   cluster = c(1, 1, 2, 2, 2)
#' )
#'
#' # Set cluster labels
#' GC_chart(genes_data, cluster ="cluster", group = "group") %>%
#' GC_clusterLabel(title = unique(genes_data$cluster))
#'
#' # Set label for a specific cluster
#' GC_chart(genes_data, cluster ="cluster", group = "group") %>%
#' GC_clusterLabel(title = "Cluster 1", cluster = 1)
#'
#' # Style labels
#' GC_chart(genes_data, cluster ="cluster", group = "group") %>%
#'   GC_clusterLabel(
#'     title = "Cluster 1",
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

    # Set width for each cluster based on position
    if (currentPosition == "left") {
      GC_chart$x$series[[clusters[i]]]$grid$margin$left <- width[(i-1) %% length(width) + 1]
    } else if (currentPosition == "right") {
      GC_chart$x$series[[clusters[i]]]$grid$margin$right <- width[(i-1) %% length(width) + 1]
    }

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
#' @param ... Additional arguments for further customization of the footers.
#'
#' @return A GC chart object with updated footer settings for each specified cluster.
#'
#' @examples
#' genes_data <- data.frame(
#'   start = c(10, 90, 130, 170, 210),
#'   stop = c(40, 120, 160, 200, 240),
#'   name = c('Gene 1', 'Gene 3', 'Gene 4', 'Gene 5', 'Gene 6'),
#'   group = c('A', 'B', 'B', 'A', 'C'),
#'   cluster = c(1, 1, 2, 2, 2)
#' )
#'
#' # Add a simple footer with subtitle to all clusters
#' GC_chart(genes_data, cluster = "cluster", group = "group") %>%
#'   GC_footer(
#'     title = "Cluster Footer",
#'     subtitle = "Cluster subtitle"
#'   )
#' # Add styling to the title and sub title
#' GC_chart(genes_data, cluster = "cluster", group = "group") %>%
#'   GC_footer(
#'     title = "This is a footer",
#'     subtitle = "Subtitle for the footer",
#'     height = "15px",
#'     spacing = 15,
#'     show = TRUE,
#'     cluster = 1,
#'     x = 6,
#'     y = -20,
#'     position = "middle", # right / middle
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
GC_footer <- function(
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
#' @param ... Additional arguments for further customization of the labels.
#'
#' @return A `GC chart` object with updated label settings for each specified cluster.
#'
#' @examples
#' genes_data <- data.frame(
#'   start = c(10, 90, 130, 170, 210),
#'   stop = c(40, 120, 160, 200, 240),
#'   name = c('Gene 1', 'Gene 3', 'Gene 4', 'Gene 5', 'Gene 6'),
#'   group = c('A', 'B', 'B', 'A', 'C'),
#'   cluster = c(1, 1, 2, 2, 2)
#' )
#'
#' # Add labels to all clusters
#' GC_chart(genes_data, cluster = "cluster", group = "group") %>%
#' GC_labels()
#'
#' # Add labels and styling
#' GC_chart(genes_data, cluster = "cluster", group = "group") %>%
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
#' GC_chart(genes_data, cluster = "cluster", group = "group") %>%
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

  return(GC_chart)
}

#' Modify Coordinates in a GC Chart
#'
#' This function updates a GC chart by modifying the coordinates settings.
#' It allows for showing or hiding tick values, applying custom tick values for the top and bottom axes,
#' and supports several other customizations for specific or all clusters in the chart.
#'
#' @param GC_chart The GC chart object to be modified.
#' @param show Logical, whether to show the tick values or not. Can be a single value or a vector.
#' @param tickValuesTop Numeric vector or NULL, custom tick values to be used at the top of the cluster.
#'                      If NULL, the default tick values are used.
#' @param tickValuesBottom Numeric vector or NULL, custom tick values to be used at the bottom of the cluster.
#'                         If NULL, the default tick values are used.
#' @param cluster Numeric or character, specifies the clusters to be affected by the coordinate modifications.
#'                If NULL, applies to all clusters.
#' @param ... Additional arguments to be passed to the coordinate options.
#'
#' @return Returns the GC chart object with updated coordinates.
#'
#' @examples
#' genes_data <- data.frame(
#'   start = c(10, 90, 130, 170, 210),
#'   stop = c(40, 120, 160, 200, 240),
#'   name = c('Gene 1', 'Gene 2', 'Gene 3', 'Gene 4', 'Gene 5'),
#'   cluster = c(1, 1, 2, 2, 2)
#' )
#'
#' # Add coordinates to all clusters
#' GC_chart(genes_data, cluster = "cluster", group = "name") %>%
#' GC_coordinates()
#'
#' # Modify coordinates of a specific cluster
#' GC_chart(genes_data, cluster = "cluster", group = "name") %>%
#' GC_coordinates() %>%
#' GC_coordinates(
#'   cluster = 2,
#'   show = TRUE,
#'   tickValuesTop = c(130, 170, 210, 240),
#'   tickValuesBottom = c(160, 200),
#'   rotate = -45,
#'   yPositionTop = 55,
#'   yPositionBottom = 45,
#'   overlapPercentage = 2,
#'   tickStyle = list(stroke = "black", strokeWidth = 1, lineLength = 6),
#'   textStyle = list(fill = "black", fontSize = "12px", fontFamily = "Arial", cursor = "default")
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
#' @param ... Additional arguments to be passed to the gene options.
#'
#' @return Returns the modified gene chart object.
#'
#' @examples
#' genes_data <- data.frame(
#'   start = c(10, 90, 130, 170, 210),
#'   stop = c(40, 120, 160, 200, 240),
#'   name = c('Gene 1', 'Gene 3', 'Gene 4', 'Gene 5', 'Gene 6'),
#'   group = c('A', 'B', 'B', 'A', 'C'),
#'   cluster = c(1, 1, 2, 2, 2)
#' )
#'
#' # Change the appearance of a specific gene cluster
#' GC_chart(genes_data, cluster = "cluster", group = "group") %>%
#'   GC_genes(
#'     group = "group",
#'     show = TRUE,
#'     colorScheme = NULL, # One of D3.js build in colorSchemes
#'                         # (eg. "schemeCategory10",
#'                         # "schemeAccent", "schemeTableau10")
#'     customColors = NULL, # A vector of color names
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
#'
#' # Change the appearance of a specific gene
#' GC_chart(genes_data, cluster = "cluster", group = "group") %>%
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
    ...
) {

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

  # Capture arguments
  dots <- list(...)

  # Update the GC_chart object with title and options for each cluster
  clusters <- getUpdatedClusters(GC_chart, cluster)

  for(i in seq_along(clusters)){

    # Default options
    options <- list(
      group = group[(i-1) %% length(group) + 1],
      show = show[(i-1) %% length(show) + 1],
      colorScheme = colorScheme,
      customColors = customColors
    )

    # Add ... arguments to defaultOptions
    for(name in names(dots)) {
      options[[name]] <- dots[[name]][(i-1) %% length(dots[[name]]) + 1]
    }

    GC_chart$x$series[[clusters[i]]]$genes <- options

  }

  return(GC_chart)
}

#' Set Legend for a Gene Chart
#'
#' This function configures the legend for a gene chart. It can toggle the legend's visibility,
#' apply a color scheme, assign custom colors, and set custom labels for the legend entries.
#'
#' @param GC_chart The gene chart object to be modified.
#' @param group Optional; character or NULL, the groups to include in the legend. If NULL,
#'   the groups are taken from the 'group' attribute of the 'GC_chart' object.
#' @param show Logical, whether to display the legend or not.
#' @param colorScheme Optional; character or NULL, the name of the color scheme to apply to the legend.
#'   This can be one of D3.js's built-in color schemes (e.g., "schemeCategory10",
#'   "schemeAccent", "schemeTableau10").
#' @param customColors Optional; list or NULL, custom colors to apply to the legend entries.
#' @param labels Optional; character or NULL, custom labels for the legend entries.
#' @param ... Additional arguments to be passed to the legend configuration.
#'
#' @return Returns the gene chart object with the legend configured.
#'
#' @examples
#' genes_data <- data.frame(
#'   start = c(10, 90, 130, 170, 210),
#'   stop = c(40, 120, 160, 200, 240),
#'   name = c('Gene 1', 'Gene 3', 'Gene 4', 'Gene 5', 'Gene 6'),
#'   group = c('A', 'B', 'B', 'A', 'C'),
#'   cluster = c('A', 'A', 'A', 'B', 'B')
#' )
#'
#' # Assuming GC_chart is a function that creates a gene chart object
#' GC_chart(genes_data, cluster = "cluster", group = "group") %>%
#'   GC_legend(
#'     position = "top", #bottom
#'     orientation = "horizontal", #vertical
#'     x = 10,
#'     y = 10,
#'     adjustHeight = TRUE,
#'     labels = NULL, # c('Group A', 'Group B', 'Group C'),
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
#'     ),
#'   )
#'
#' # Configure the legend of the gene chart
#' GC_chart(genes_data, cluster = "cluster", group = "group") %>%
#'   GC_legend(
#'     show = TRUE,
#'     colorScheme = 'schemeCategory10', # Example color scheme
#'     customColors = list('A' = 'blue', 'B' = 'green', 'C' = 'red'),
#'     labels = c('Group A', 'Group B', 'Group C')
#'   )
#'
#'
#' @export
GC_legend <- function(
    GC_chart,
    group = NULL,
    show = TRUE,
    colorScheme = NULL,
    customColors = NULL,
    backgroundColor = "white",
    labels = NULL,
    ...
) {

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

  # Get the names of the clusters
  clusters <- names(GC_chart$x$series)

  # Capture arguments
  dots <- list(...)

  # Default options
  options <- list(
    group = group,
    show = show,
    colorScheme = colorScheme,
    customColors = customColors,
    backgroundColor = backgroundColor,
    labels = labels
  )

  # Add ... arguments to defaultOptions
  for(name in names(dots)) {
    options[[name]] <- dots[[name]]
  }

  GC_chart$x$legend <- options

  return(GC_chart)
}

#' Set Tooltip for a Gene Chart
#'
#' This function configures the tooltip for a gene chart.
#'
#' @param GC_chart The gene chart object to be modified.
#' @param formatter A character string defining the HTML content of the tooltip. It can
#'   include placeholders like `{start}` and `{stop}` which will be replaced by actual
#'   data values. The default value shows start and stop data.
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
#'   stop = c(40, 120, 160, 200, 240),
#'   name = c('Gene 1', 'Gene 3', 'Gene 4', 'Gene 5', 'Gene 6'),
#'   group = c('A', 'B', 'B', 'A', 'C')
#' )
#'
#' # Add tooltips to the gene chart
#' GC_chart(genes_data, cluster = "cluster", group = "group") %>%
#' GC_tooltip(formatter = " <b>Start:</b> {start}<br><b>Stop:</b> {stop}")
#'
#' @export
GC_tooltip <- function(
    GC_chart,
    formatter = "<b>Start:</b> {start}<br><b>Stop:</b> {stop}",
    show = TRUE,
    cluster = NULL,
    ...
) {

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
