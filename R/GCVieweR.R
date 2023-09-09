#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`

#' Create a GC Chart Visualization
#'
#' Generates an interactive GC chart for genomic data.
#'
#' @param data Data frame with genomic data.
#' @param start_col Start positions column. Default: "start".
#' @param stop_col Stop positions column. Default: "stop".
#' @param cluster Optional cluster column.
#' @param group Column to determine gene groups. Used for Aesthetic mapping of genes to colors.
#' @param width Chart width, e.g., "100%" or 500.
#' @param height Chart height, e.g., "400px" or 300.
#' @param elementId Optional widget ID.
#' @param scale_breaks Use scale breaks? Default: TRUE.
#' @param scale_break_threshold % of full range to determine inter-gene regions for scale breaks.
#' @param scale_break_padding % of full range as padding on both sides of a scale break.
#'
#' @return GC chart widget.
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
GC_chart <- function(data, start_col = "start", stop_col = "stop", cluster = NULL, group = NULL,
                     width = "100%", height = "400px", elementId = NULL, scale_breaks = TRUE, scale_break_threshold = 20, scale_break_padding = 1){

  # ensure that data is a data frame
  stopifnot(is.data.frame(data))

  # Check if column names are in the data frame
  colnames_data <- colnames(data)
  if (!(start_col %in% colnames_data)) stop("start column not found in data")
  if (!(stop_col %in% colnames_data)) stop("stop column not found in data")
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
  x$legend <- list(group = group, show = show_legend, position = "top")

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

    subset_data$start <- subset_data[[start_col]]
    subset_data$stop <- subset_data[[stop_col]]
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

    x$series[[clust]]$grid <- list(margin = list(left = "50px", right = "50px", top = 0, bottom = 0), height = height, width = width)
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
#' "labels",
#' cluster = 2,  # If NULL all clusters will be updated
#' fontSize = "16px"
#' )
#'
#' # Update style of specific items within a specific cluster
#' GC_chart(genes_data, cluster ="cluster", group = "group") %>%
#' GC_labels("name") %>%
#' GC_item(
#' "labels",
#' cluster = 2, # Can be index or name of cluster
#' itemStyle =
#'  list(
#'    list(index = 0, fontSize = "10px"), # Note index 0 is used
#'    list(index = 2, fontSize = "16px"). # because of JS array.
#'    )
#'  )
#'
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
    height = "40px",
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

#' @export
GC_sequence <- function(
    GC_chart,
    show = TRUE,
    option = NULL,
    cluster = NULL,
    ...
) {

  # Capture ... arguments
  dots <- list(...)

  GC_item(GC_chart, "sequence", cluster = cluster, dots)

  # Update the GC_chart object with title and options for each cluster
  clusters <- getUpdatedClusters(GC_chart, cluster)

  for(i in seq_along(clusters)){

    # Default options
    options <- list(
      show = show[(i-1) %% length(show) + 1]
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

#' @export
GC_grid <- function(
    GC_chart,
    margin = NULL,
    width = NULL,
    height = NULL,
    cluster = NULL
) {

  # Update the GC_chart object with title and options for each cluster
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
      GC_chart$x$series[[cluster_name]]$grid$width <- current_width
    }

    # Update height if provided
    if (!is.null(height)) {
      current_height <- height[(i-1) %% length(height) + 1]
      # Convert numeric height to percentage string
      if (is.numeric(current_height)) {
        current_height <- paste0(current_height, "%")
      }
      GC_chart$x$series[[cluster_name]]$grid$height <- current_height
    }
  }

  return(GC_chart)
}

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



#' @export
GC_scaleBar <- function(
    GC_chart,
    show = TRUE,
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
      show = show[(i-1) %% length(show) + 1]
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

#' @export
GC_clusterLabel <- function(
    GC_chart,
    title = NULL,
    show = TRUE,
    width = "100px",
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
      show = show[(i-1) %% length(show) + 1]
    )

    # Add ... arguments to options
    for(name in names(dots)) {
      options[[name]] <- dots[[name]][(i-1) %% length(dots[[name]]) + 1]
    }

    # Set clusterLabel options for each cluster
    GC_chart$x$series[[clusters[i]]]$clusterLabel <- options

    # Set width for each cluster
    GC_chart$x$series[[clusters[i]]]$grid$margin$left <- width[(i-1) %% length(width) + 1]

  }

  return(GC_chart)
}

#' @export
GC_footer <- function(
    GC_chart,
    title = NULL,
    subtitle = NULL,
    height = NULL,
    show = TRUE,
    cluster = NULL,
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
      show = show[(i-1) %% length(show) + 1]
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

#' @export
GC_labels <- function(
    GC_chart,
    label = NULL,
    show = TRUE,
    cluster = NULL,
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
      show = show[(i-1) %% length(show) + 1]
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

#' @export
GC_coordinates <- function(
    GC_chart,
    show = TRUE,
    tickValues = NULL,
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
      show = show[(i-1) %% length(show) + 1]
    )

    # Add ... arguments to options
    for(name in names(dots)) {
      options[[name]] <- dots[[name]][(i-1) %% length(dots[[name]]) + 1]
    }

    # Set coordinates options for each cluster
    GC_chart$x$series[[clusters[i]]]$coordinates <- options

    # Add tickvalues for each cluster
    GC_chart$x$series[[clusters[i]]]$coordinates$tickValues <- tickValues

  }

  return(GC_chart)
}

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

#' @export
GC_legend <- function(
    GC_chart,
    group = NULL,
    show = TRUE,
    colorScheme = NULL,
    customColors = NULL,
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
    labels = labels
  )

  # Add ... arguments to defaultOptions
  for(name in names(dots)) {
    options[[name]] <- dots[[name]]
  }

  GC_chart$x$legend <- options

  return(GC_chart)
}

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
