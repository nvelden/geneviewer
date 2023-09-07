#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`

#' @import htmlwidgets
#' @export
GCVieweR <- function(data, start_col = "start", stop_col = "stop", cluster = NULL, group = NULL,
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
      subset_data$start <- subset_data[[start_col]]
      subset_data$stop <- subset_data[[stop_col]]
      subset_data <- subset_data[with(subset_data, order(pmax(start, stop), decreasing = TRUE)), ]
      subset_data$cluster <- clust
    }

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
    x$series[[clust]]$markers <- list(group = group, show = TRUE)
    x$series[[clust]]$genes <- list(group = group, show = TRUE)
    x$series[[clust]]$labels <- list()
    x$series[[clust]]$cluster <- list()
    x$series[[clust]]$coordinates <- list(show = TRUE)
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
#GC_item("markers", 1, itemStyle = list(list(index = 1, styles = list(opacity = 0.1))))

#' @export
GC_item <- function(
    GCVieweR,
    setting = NULL,
    cluster = NULL,
    ...
) {

  # Capture ... arguments
  dots <- list(...)

  # Update the GCVieweR object with sequence options for each cluster
  clusters <- getUpdatedClusters(GCVieweR, cluster)

  for(clust in cluster){

    settings <- GCVieweR$x$series[[clust]][[setting]]
    updated_settings <- modifyList(settings, dots)

    # Set options for each cluster
    GCVieweR$x$series[[clust]][[setting]] <- updated_settings

  }

  return(GCVieweR)

}

#' @export
GC_title <- function(
    GCVieweR,
    title = NULL,
    subtitle = NULL,
    show = TRUE,
    height = "40px",
    cluster = NULL,
    ...
) {

  # Capture ... arguments
  dots <- list(...)

  # Update the GCVieweR object with title and options for each cluster
  clusters <- getUpdatedClusters(GCVieweR, cluster)

  for(i in seq_along(clusters)){

    # Default options
    options <- list(
      title = title[(i-1) %% length(title) + 1],
      subtitle = subtitle[(i-1) %% length(subtitle) + 1],
      show = show
    )

    # Add arguments to options
    for(name in names(dots)) {
      options[[name]] <- dots[[name]][(i-1) %% length(dots[[name]]) + 1]
    }

    # Set height for each cluster
    GCVieweR$x$series[[clusters[i]]]$grid$margin$top <- height[(i-1) %% length(height) + 1]

    # Set title options for each cluster
    GCVieweR$x$series[[clusters[i]]]$title <- options

  }
  return(GCVieweR)
}

#' @export
GC_sequence <- function(
    GCVieweR,
    show = TRUE,
    option = NULL,
    cluster = NULL,
    ...
) {

  # Capture ... arguments
  dots <- list(...)

  GC_item(GCVieweR, "sequence", cluster = cluster, dots)

  # Update the GCVieweR object with title and options for each cluster
  clusters <- getUpdatedClusters(GCVieweR, cluster)

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
    GCVieweR$x$series[[clusters[i]]]$sequence <- options

  }

  return(GCVieweR)
}

#' @export
GC_grid <- function(
    GCVieweR,
    margin = NULL,
    width = NULL,
    height = NULL,
    cluster = NULL
) {

  # Update the GCVieweR object with title and options for each cluster
  clusters <- getUpdatedClusters(GCVieweR, cluster)

  for (i in seq_along(clusters)) {
    cluster_name <- clusters[i]

    # Update margins if provided
    if (!is.null(margin)) {
      GCVieweR$x$series[[cluster_name]]$grid$margin <- modifyList(GCVieweR$x$series[[cluster_name]]$grid$margin, margin)
    }

    # Update width if provided
    if (!is.null(width)) {
      current_width <- width[(i-1) %% length(width) + 1]
      # Convert numeric width to percentage string
      if (is.numeric(current_width)) {
        current_width <- paste0(current_width, "%")
      }
      GCVieweR$x$series[[cluster_name]]$grid$width <- current_width
    }

    # Update height if provided
    if (!is.null(height)) {
      current_height <- height[(i-1) %% length(height) + 1]
      # Convert numeric height to percentage string
      if (is.numeric(current_height)) {
        current_height <- paste0(current_height, "%")
      }
      GCVieweR$x$series[[cluster_name]]$grid$height <- current_height
    }
  }

  return(GCVieweR)
}

#' @export
GC_scale <- function(
    GCVieweR,
    cluster = NULL,
    start = NULL,
    stop = NULL,
    breaks = list(),
    ...
) {

  # Capture ... arguments
  dots <- list(...)

  # Update the GCVieweR object with title and options for each cluster
  clusters <- getUpdatedClusters(GCVieweR, cluster)

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
    GCVieweR$x$series[[clusters[i]]]$scale <- options

  }

  return(GCVieweR)
}



#' @export
GC_scaleBar <- function(
    GCVieweR,
    show = TRUE,
    cluster = NULL,
    ...
) {

  # Capture ... arguments
  dots <- list(...)

  # Update the GCVieweR object with title and options for each cluster
  clusters <- getUpdatedClusters(GCVieweR, cluster)

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
    GCVieweR$x$series[[clusters[i]]]$scaleBar <- options

  }

  return(GCVieweR)
}

#' @export
GC_clusterLabel <- function(
    GCVieweR,
    title = NULL,
    show = TRUE,
    width = "100px",
    cluster = NULL,
    ...
) {

  # Capture ... arguments
  dots <- list(...)

  # Update the GCVieweR object with title and options for each cluster
  clusters <- getUpdatedClusters(GCVieweR, cluster)

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
    GCVieweR$x$series[[clusters[i]]]$clusterLabel <- options

    # Set width for each cluster
    GCVieweR$x$series[[clusters[i]]]$grid$margin$left <- width[(i-1) %% length(width) + 1]

  }

  return(GCVieweR)
}

#' @export
GC_footer <- function(
    GCVieweR,
    title = NULL,
    subtitle = NULL,
    height = NULL,
    show = TRUE,
    cluster = NULL,
    ...
) {

  # Capture ... arguments
  dots <- list(...)

  # Update the GCVieweR object with title and options for each cluster
  clusters <- getUpdatedClusters(GCVieweR, cluster)

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
    GCVieweR$x$series[[clusters[i]]]$footer <- options

    # Set height for each cluster
    GCVieweR$x$series[[clusters[i]]]$grid$bottom <- currentHeight

  }

  return(GCVieweR)
}

#' @export
GC_labels <- function(
    GCVieweR,
    label = NULL,
    show = TRUE,
    cluster = NULL,
    ...
) {

  if (is.logical(label) && length(label) == 1) {
    show <- label
    label <- NULL
  }

  if (is.null(label) && is.null(GCVieweR$x$group)){
    stop("Please define labels")
  }

  if (is.null(label) && !is.null(GCVieweR$x$group)) {
    label <- GCVieweR$x$group
  }

  if (!(label %in% names(GCVieweR$x$data))) {
    stop("label column not found in data")
  }

  # Capture ... arguments
  dots <- list(...)

  # Update the GCVieweR object with title and options for each cluster
  clusters <- getUpdatedClusters(GCVieweR, cluster)

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
    GCVieweR$x$series[[clusters[i]]]$labels <- options

  }

  return(GCVieweR)
}

#' @export
GC_coordinates <- function(
    GCVieweR,
    show = TRUE,
    tickValues = NULL,
    cluster = NULL,
    ...
) {

  # Capture ... arguments
  dots <- list(...)

  # Update the GCVieweR object with title and options for each cluster
  clusters <- getUpdatedClusters(GCVieweR, cluster)

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
    GCVieweR$x$series[[clusters[i]]]$coordinates <- options

    # Add tickvalues for each cluster
    GCVieweR$x$series[[clusters[i]]]$coordinates$tickValues <- tickValues

  }

  return(GCVieweR)
}

#' @export
#' @export
GC_genes <- function(
    GCVieweR,
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

  if (is.null(group) && is.null(GCVieweR$x$group)){
    stop("Please define a group")
  }

  if (is.null(group) && !is.null(GCVieweR$x$group)){
    group <- GCVieweR$x$group
  }

  if (!(group %in% names(GCVieweR$x$data))) {
    stop("group column not found in data")
  }

  # Capture arguments
  dots <- list(...)

  # Update the GCVieweR object with title and options for each cluster
  clusters <- getUpdatedClusters(GCVieweR, cluster)

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

    GCVieweR$x$series[[clusters[i]]]$genes <- options

  }

  return(GCVieweR)
}

#' @export
GC_markers <- function(
    GCVieweR,
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

  if (is.null(group) && is.null(GCVieweR$x$group)){
    stop("Please define a group")
  }

  if (is.null(group) && !is.null(GCVieweR$x$group)){
    group <- GCVieweR$x$group
  }

  if (!(group %in% names(GCVieweR$x$data))) {
    stop("group column not found in data")
  }

  # Capture arguments
  dots <- list(...)

  # Update the GCVieweR object with title and options for each cluster
  clusters <- getUpdatedClusters(GCVieweR, cluster)

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

    GCVieweR$x$series[[clusters[i]]]$markers <- options

  }

  return(GCVieweR)
}

#' @export
GC_legend <- function(
    GCVieweR,
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

  if (is.null(group) && is.null(GCVieweR$x$group) && is.null(labels)){
    stop("Please define a group")
  }

  if (is.null(group) && !is.null(GCVieweR$x$group)){
    group <- GCVieweR$x$group
  }

  if (is.null(labels) && !(group %in% names(GCVieweR$x$data))) {
    stop("group column not found in data")
  }

  # Get the names of the clusters
  clusters <- names(GCVieweR$x$series)

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

  GCVieweR$x$legend <- options

  return(GCVieweR)
}

#' @export
GC_tooltip <- function(
    GCVieweR,
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

  # Update the GCVieweR object with title and options for each cluster
  clusters <- getUpdatedClusters(GCVieweR, cluster)

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
    GCVieweR$x$series[[clusters[i]]]$tooltip <- options

  }
  return(GCVieweR)
}
