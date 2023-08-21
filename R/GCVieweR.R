#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`


# check_columns_in_data <- function(data, group = NULL) {
#
#   group_eval <- enquo(group)
#   group_char <- quo_name(group_eval)
#
#   # Check if the string names are present in the data's column names
#   colnames_data <- colnames(data)
#
#   if (group_char != "NULL" && !(group_char %in% colnames_data)) {
#     stop("group column not found in data")
#   }
#
#   selection <- if((group_char != "NULL")) data[[group_char]] else NULL
#
#   return(selection)
# }
#
#
# data_example <- data.frame(value = 1:5)
#
# check_columns_in_data(data_example, value)
# check_columns_in_data(data_example)
# check_columns_in_data(data_example, x)



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

  x$data <- data
  x$grid <- list(left = "50px", right = "50px", top = 0, bottom = 0)
  x$legend <- list(group = group_char, show = TRUE, position = "top")

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
    # Settings
    x$series[[clust]]$title <- list()
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

#' @export
GC_sequence <- function(
    GCVieweR,
    show = TRUE,
    yOffset = 50,
    start = NULL,
    stop = NULL,
    options = list()
) {

  # Default options
  defaultOptions <- list(
    show = show,
    y = yOffset,
    start = start,
    stop = stop
  )

  # Merge user-specified options with defaults
  opts <- modifyList(defaultOptions, options)

  # Update the GCVieweR object with scaleBar options
  GCVieweR$x$GC_sequence$options <- opts

  return(GCVieweR)
}

#' export
GC_grid <- function(
    GCVieweR,
    left = NULL,
    right = NULL,
    top = NULL,
    bottom = NULL) {

  margins <- list(left = left, right = right, top = top, bottom = bottom)

  for (name in names(margins)) {
    if (!is.null(margins[[name]])) {
      GCVieweR$x$grid[[name]] <- margins[[name]]
    }
  }

  return(GCVieweR)
}

#' @export
GC_scaleBar <- function(
    GCVieweR,
    show = TRUE,
    title = "1 kb",
    scaleBarUnit = 1000,
    yOffset = 50,
    options = list()
) {

  # Default options
  defaultOptions <- list(
    show = show,
    title = title,
    y = yOffset,
    scaleBarUnit = scaleBarUnit
  )

  # Merge user-specified options with defaults
  opts <- modifyList(defaultOptions, options)

  # Update the GCVieweR object with scaleBar options
  GCVieweR$x$GC_scaleBar$options <- opts

  return(GCVieweR)
}

#' @export
GC_clusterLabel <- function(
    GCVieweR,
    title = NULL,
    show = TRUE,
    position = "left",
    xOffset = 0,
    yOffset = 0,
    width = "100px",
    wrapLabel = TRUE,
    options = list()
) {

  # Default font options
  defaultOptions <- list(
    title = title,
    show = show,
    wrapLabel = wrapLabel,
    position = position,
    x = xOffset,
    y = yOffset
  )

  GCVieweR$x$GC_grid$left <- width

  # Merge user-specified font options with defaults
  opts <- modifyList(defaultOptions, options)

  # Update the GCVieweR object with labels and options
  GCVieweR$x$GC_clusterLabel$options <- opts

  return(GCVieweR)
}

#' @export
GC_footer <- function(
    GCVieweR,
    title = NULL,
    subtitle = NULL,
    position = "left",
    xOffset = -10,
    yOffset = 20,
    spacing = 15,
    options = list()
) {

  # Default font options
  defaultOptions <- list(
    title = title,
    subtitle = subtitle,
    position = position,
    spacing = spacing,
    x = xOffset,
    y = yOffset
  )

  # Merge user-specified options with defaults
  opts <- modifyList(defaultOptions, options)

  # Update the GCVieweR object with labels and options
  GCVieweR$x$GC_footer$options <- opts

  return(GCVieweR)
}

#' @export
GC_labels <- function(
    GCVieweR,
    label,
    y = 50,
    start = NULL,
    stop = NULL,
    anchor = "middle",
    dy = "-1em",
    dx = "0em",
    x = 0,
    rotate = 0,
    adjustLabels = TRUE,
    options = list()
) {

  # Check if labels are provided
  if (is.null(labels)) {
    stop("Labels must be provided.")
  }

  label <- deparse(substitute(label))

  # Default options
  defaultOptions <- list(
    label = label,
    y = y,
    start = start,
    stop = stop,
    anchor = anchor,
    dy = dy,
    dx = dx,
    x = x,
    adjustLabels = adjustLabels,
    rotate = rotate,
    options = list()
  )

  # Merge user-specified options with defaults
  opts <- modifyList(defaultOptions, options)

  # Update the GCVieweR object with labels and options
  GCVieweR$x$GC_labels$options <- opts

  return(GCVieweR)
}

#' @export
GC_coordinates <- function(
    GCVieweR,
    show = TRUE,
    options = list()
) {

  # Default options
  defaultOptions <- list(
    show = show
  )

  # Merge user-specified options with defaults
  opts <- modifyList(defaultOptions, options)

  # Update the GCVieweR object with coordinates and options
  GCVieweR$x$GC_coordinates$options <- opts

  return(GCVieweR)
}

#' @export
GC_genes <- function(
    GCVieweR,
    group,
    show = TRUE,
    y = 50,
    start = NULL,
    stop = NULL,
    colorScheme = NULL,
    customColors = NULL,
    marker = "doubleArrow",
    markerSize = 10,
    strokeWidth = 1,
    opacity = 1,
    options = list()
) {

  group <- deparse(substitute(group))

  # Default options
  defaultOptions <- list(
    group = group,
    show = show,
    y = y,
    start = start,
    stop = stop,
    colorScheme = colorScheme,
    customColors = customColors,
    marker = marker,
    markerSize = markerSize,
    strokeWidth = strokeWidth,
    opacity = opacity,
    options = list()
  )

  # Merge user-specified options with defaults
  opts <- modifyList(defaultOptions, options)

  # Update the GCVieweR object with genes and options
  GCVieweR$x$GC_genes$options <- opts

  return(GCVieweR)
}

#' @export
GC_title <- function(
    GCVieweR,
    title = NULL,
    subtitle = NULL,
    show = TRUE,
    height = "50px",
    spacing = 10,
    position = "center",
    xOffset = 10,
    yOffset = 0,
    options = list()
) {

  GCVieweR$x$grid$top <- height

  # Update the GCVieweR object with title and options for each cluster
  clusters <- names(GCVieweR$x$series)

  for(i in seq_along(clusters)){

    # Default options
    defaultOptions <- list(
      title = title[(i-1) %% length(title) + 1],
      show = show[(i-1) %% length(show) + 1],
      subtitle = subtitle[(i-1) %% length(subtitle) + 1],
      position = position[(i-1) %% length(position) + 1],
      spacing = spacing[(i-1) %% length(spacing) + 1],
      x = xOffset[(i-1) %% length(xOffset) + 1],
      y = yOffset[(i-1) %% length(yOffset) + 1]
    )

    # Recycle options for each cluster
    recycledOptions <- lapply(options, function(opt) {
      opt[(i-1) %% length(opt) + 1]
    })

    # Merge user-specified options with defaults
    opts <- modifyList(defaultOptions, recycledOptions)

    GCVieweR$x$series[[clusters[i]]]$title <- opts

  }

  return(GCVieweR)
}

#' @export
GC_legend <- function(
    GCVieweR,
    group = NULL,
    show = TRUE,
    position = "top",
    orientation = "horizontal",
    height = NULL,
    xOffset = 10,
    yOffset = 10,
    margin = list(top = 0, right = 0, bottom = 0, left = 0),
    backgroundColor = "#FFF",
    options = list()
) {

  group_eval <- rlang::enquo(group)
  group_char <- rlang::quo_name(group_eval)

  if (group_char == "NULL" && GCVieweR$x$legend$group == "NULL"){
    stop("Please define a group")
  }

  if (!(group_char %in% names(GCVieweR$x$data))) {
    stop("group column not found in data")
  }

  if(group_char == "NULL" && GCVieweR$x$legend$group != "NULL"){
    group_char <- GCVieweR$x$legend$group
  }

  # Define default options
  defaultOptions <- list(
    x = xOffset,
    y = yOffset,
    group = group_char,
    show = show,
    margin = margin,
    height = height,
    position = position,
    backgroundColor = backgroundColor,
    orientation = orientation
  )

  # Merge user-specified options with defaults
  opts <- modifyList(defaultOptions, options)

  GCVieweR$x$legend <- opts

  return(GCVieweR)
}
