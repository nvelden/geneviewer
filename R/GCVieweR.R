#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`

#' @import htmlwidgets
#' @export
GCVieweR <- function(data, start = start, stop = stop, cluster = NULL, group = NULL,
                     width = "100%", height = NULL, elementId = NULL){

  # ensure that data is a data frame
  stopifnot(is.data.frame(data))

  # use deparse(substitute(...)) to capture the column names
  start_col <- deparse(substitute(start))
  stop_col <- deparse(substitute(stop))

  cluster_eval <- rlang::enquo(cluster)
  cluster_char <- rlang::quo_name(cluster_eval)

  group_eval <- rlang::enquo(group)
  group_char <- rlang::quo_name(group_eval)

  # Check if column names are in the data frame
  colnames_data <- colnames(data)
  if (!(start_col %in% colnames_data)) stop("start column not found in data")
  if (!(stop_col %in% colnames_data)) stop("stop column not found in data")
  if (cluster_char != "NULL" && !(cluster_char %in% colnames_data)){
    stop("cluster column not found in data")
  }
  if (group_char != "NULL" && !(group_char %in% colnames_data)) {
    stop("group column not found in data")
  }

  x <- list()

  group <- if((group_char != "NULL")) group_char else NULL
  show_legend <- if((group_char != "NULL")) TRUE else FALSE

  x$data <- data
  x$group <- group
  x$legend <- list(group = group, show = show_legend, position = "top")

  cluster <- if((cluster_char != "NULL")) cluster_char else NULL

  if(is.null(cluster)){
    clusters <- "cluster"
  } else {
    if (!(cluster %in% colnames_data)) stop("cluster column not found in data")
    clusters <- unique(data[[cluster]])
  }

  for(clust in clusters){

    # Subset data for the current cluster
    if(is.null(cluster)){
      subset_data <- data
    } else {
      subset_data <- data[data[[cluster_char]] == clust, ]
    }

    # Data
    x$series[[clust]]$clusterName <- clust
    x$series[[clust]]$data <- subset_data
    x$series[[clust]]$data$start <- subset_data[[start_col]]
    x$series[[clust]]$data$stop <- subset_data[[stop_col]]
    x$series[[clust]]$data$cluster <- clust
    # Settings
    x$series[[clust]]$grid <- list(left = "50px", right = "50px", top = 0, bottom = 0)
    x$series[[clust]]$title <- list()
    x$series[[clust]]$markers <- list(group = group_char, show = TRUE)
    x$series[[clust]]$genes <- list(group = group_char, show = TRUE)
    x$series[[clust]]$labels <- list()
    x$series[[clust]]$cluster <- list()
    x$series[[clust]]$coordinates <- list(show = TRUE)
    x$series[[clust]]$scaleBar <- list()
    x$series[[clust]]$footer <- list()
    x$series[[clust]]$clusterLabel <- list()
    x$series[[clust]]$sequence <- list(show = TRUE)
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
  clusters <- names(GCVieweR$x$series)

  # If cluster is NULL, update all clusters
  if (is.null(cluster)) {
    cluster <- clusters
  } else if (is.numeric(cluster)) {
    # If cluster is numeric, map the numbers to cluster names
    if (any(cluster > length(clusters) | cluster < 1)) {
      warning("Some cluster numbers provided are out of range. Please check the cluster numbers.")
      return(GCVieweR)
    }
    cluster <- clusters[cluster]
  } else if (!all(cluster %in% clusters)) {
    warning("Some cluster names provided are not valid. Please check the cluster names.")
    return(GCVieweR)
  } else {
    warning("The cluster argument has an incorrect format. Please provide valid cluster names or numbers.")
    return(GCVieweR)
  }

  for(clust in cluster){

    settings <- GCVieweR$x$series[[clust]][[setting]]
    updated_settings <- modifyList(settings, dots)

    # Set sequence options for each cluster
    GCVieweR$x$series[[clust]][[setting]] <- updated_settings

  }

  return(GCVieweR)

}

#' @export
GC_sequence <- function(
    GCVieweR,
    show = TRUE,
    option = NULL,
    ...
) {

  # Capture ... arguments
  dots <- list(...)

  # Update the GCVieweR object with sequence options for each cluster
  clusters <- names(GCVieweR$x$series)

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

#' export
GC_grid <- function(GCVieweR, left = NULL, right = NULL, top = NULL, bottom = NULL) {

  # Initialize margins list
  margins <- list(left = left, right = right, top = top, bottom = bottom)
  # Extract cluster names
  clusters <- names(GCVieweR$x$series)

  for (i in seq_along(clusters)) {

    # Loop over each margin and update the respective property in the 'grid' list
    for (name in names(margins)) {
      if (!is.null(margins[[name]])) {
        GCVieweR$x$series[[clusters[i]]]$grid[[name]] <- margins[[name]]
      }
    }
  }

  return(GCVieweR)
}

#' @export
GC_scaleBar <- function(
    GCVieweR,
    show = TRUE,
    ...
) {

  # Capture ... arguments
  dots <- list(...)

  # Update the GCVieweR object with scaleBar options for each cluster
  clusters <- names(GCVieweR$x$series)

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
    ...
) {

  # Capture ... arguments
  dots <- list(...)

  # Update the GCVieweR object with clusterLabel options for each cluster
  clusters <- names(GCVieweR$x$series)

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

    # Set height for each cluster
    GCVieweR$x$series[[clusters[i]]]$grid$left <- width[(i-1) %% length(width) + 1]

  }

  return(GCVieweR)
}

#' @export
GC_footer <- function(
    GCVieweR,
    title = NULL,
    subtitle = NULL,
    show = TRUE,
    ...
) {

  # Capture ... arguments
  dots <- list(...)

  # Update the GCVieweR object with footer options for each cluster
  clusters <- names(GCVieweR$x$series)

  for(i in seq_along(clusters)){

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

  }

  return(GCVieweR)
}

#' @export
GC_labels <- function(
    GCVieweR,
    label,
    show = TRUE,
    ...
) {

  label_eval <- rlang::enquo(label)
  label_char <- rlang::quo_name(label_eval)

  if (label_char == "NULL" && is.null(GCVieweR$x$group)){
    stop("Please define labels")
  }

  if(label_char == "NULL" && !is.null(GCVieweR$x$group)){
    label_char <- GCVieweR$x$group
  }

  if (!(label_char %in% names(GCVieweR$x$data))) {
    stop("label column not found in data")
  }

  # Capture ... arguments
  dots <- list(...)

  # Update the GCVieweR object with labels options for each cluster
  clusters <- names(GCVieweR$x$series)

  for(i in seq_along(clusters)){

    # Default options
    options <- list(
      label = label_char[(i-1) %% length(label_char) + 1],
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
    ...
) {

  # Capture ... arguments
  dots <- list(...)

  # Update the GCVieweR object with coordinates and options for each cluster
  clusters <- names(GCVieweR$x$series)

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
GC_genes <- function(
    GCVieweR,
    group = NULL,
    show = TRUE,
    ...
) {

  group_eval <- rlang::enquo(group)
  group_char <- rlang::quo_name(group_eval)

  if (group_char == "NULL" && is.null(GCVieweR$x$group)){
    stop("Please define a group")
  }

  if(group_char == "NULL" && !is.null(GCVieweR$x$group)){
    group_char <- GCVieweR$x$group
  }

  if (!(group_char %in% names(GCVieweR$x$data))) {
    stop("group column not found in data")
  }

  # Capture arguments
  dots <- list(...)

  # Update the GCVieweR object with genes options for each cluster
  clusters <- names(GCVieweR$x$series)

  for(i in seq_along(clusters)){

    # Default options
    options <- list(
      group = group_char[(i-1) %% length(group_char) + 1],
      show = show[(i-1) %% length(show) + 1]
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
    ...
) {

  group_eval <- rlang::enquo(group)
  group_char <- rlang::quo_name(group_eval)

  if (group_char == "NULL" && is.null(GCVieweR$x$group)){
    stop("Please define a group")
  }

  if(group_char == "NULL" && !is.null(GCVieweR$x$group)){
    group_char <- GCVieweR$x$group
  }

  if (!(group_char %in% names(GCVieweR$x$data))) {
    stop("group column not found in data")
  }

  # Capture arguments
  dots <- list(...)

  # Update the GCVieweR object with markers options for each cluster
  clusters <- names(GCVieweR$x$series)

  for(i in seq_along(clusters)){

    # Default options
    options <- list(
      group = group_char[(i-1) %% length(group_char) + 1],
      show = show[(i-1) %% length(show) + 1]
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
GC_title <- function(
    GCVieweR,
    title,
    show = TRUE,
    height = "50px",
    ...
) {

  # Capture ... arguments
  dots <- list(...)

  # Update the GCVieweR object with title and options for each cluster
  clusters <- names(GCVieweR$x$series)

  for(i in seq_along(clusters)){

    # Default options
    options <- list(
      title = title[(i-1) %% length(title) + 1],
      show = show
    )

    # Add arguments to options
    for(name in names(dots)) {
      options[[name]] <- dots[[name]][(i-1) %% length(dots[[name]]) + 1]
    }

    # Set height for each cluster
    GCVieweR$x$series[[clusters[i]]]$grid$top <- height[(i-1) %% length(height) + 1]

    # Set title options for each cluster
    GCVieweR$x$series[[clusters[i]]]$title <- options

  }
  return(GCVieweR)
}

#' @export
GC_legend <- function(
    GCVieweR,
    group = NULL,
    show = TRUE,
    ...
) {

  group_eval <- rlang::enquo(group)
  group_char <- rlang::quo_name(group_eval)

  if (group_char == "NULL" && is.null(GCVieweR$x$group)){
    stop("Please define a group")
  }

  if(group_char == "NULL" && !is.null(GCVieweR$x$group)){
    group_char <- GCVieweR$x$group
  }

  if (!(group_char %in% names(GCVieweR$x$data))) {
    stop("group column not found in data")
  }

  clusters <- names(GCVieweR$x$series)

  # Capture arguments
  dots <- list(...)

  # Default options
  options <- list(
    group = group_char,
    show = show
  )

  # Add ... arguments to defaultOptions
  for(name in names(dots)) {
    options[[name]] <- dots[[name]]
  }

  GCVieweR$x$legend <- options

  return(GCVieweR)
}
