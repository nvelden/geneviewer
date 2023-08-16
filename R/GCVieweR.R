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

  group <- if((group_char != "NULL")) group_char else NULL
  cluster <- if((cluster_char != "NULL")) cluster_char else NULL


  # create a list of settings and data to send to the HTML widget
  x <- list()
  x$data <- data
  x$data$start <- data[[start_col]]
  x$data$stop <- data[[stop_col]]
  x$data$group <- if((group_char != "NULL")) data[[group_char]] else NULL
  # Data from functions
  x$GC_legend$options <- list(group = group, show = TRUE, position = "top")
  x$GC_title <- list()
  x$GC_genes$options <- list(group = group, show = TRUE)
  x$GC_labels <- list()
  x$GC_cluster <- list()
  x$GC_coordinates$options <- list(show = TRUE)
  x$GC_scaleBar <- list()
  x$GC_footer <- list()
  x$GC_clusterLabel <- list()
  x$GC_sequence$options <- list(show = TRUE)
  x$GC_grid <- list()

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
    left = "10%",
    right = "10%",
    top = "10%",
    bottom = "10%"){

  GCVieweR$x$GC_grid <- list(left = left, right = right, top = top, bottom = bottom)

  return(GCVieweR)

}

#' @export
GC_scaleBar <- function(
    GCVieweR,
    show = TRUE,
    title = "1 kb",
    scaleBarUnit = 1000,
    options = list()
) {

  # Default options
  defaultOptions <- list(
    show = show,
    title = title,
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
    xOffset = 10,
    yOffset = 0,
    spacing = 20,
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
    show = TRUE,
    subtitle = NULL,
    spacing = 10,
    position = "center",
    xOffset = 10,
    yOffset = 0,
    options = list()
) {

  # Default options
  defaultOptions <- list(
    title = title,
    show = show,
    subtitle = subtitle,
    position = position,
    spacing = spacing,
    x = xOffset,
    y = yOffset,
    options = list()
  )

  # Merge user-specified options with defaults
  opts <- modifyList(defaultOptions, options)

  # Update the GCVieweR object with title and options
  GCVieweR$x$GC_title$options <- opts

  return(GCVieweR)
}

#' @export
GC_legend <- function(
    GCVieweR,
    group,
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

  group <- deparse(substitute(group))

  # Define default options
  defaultOptions <- list(
    x = xOffset,
    y = yOffset,
    group = group,
    show = show,
    margin = margin,
    height = height,
    position = position,
    backgroundColor = backgroundColor,
    orientation = orientation
  )

  # Merge user-specified options with defaults
  opts <- modifyList(defaultOptions, options)

  GCVieweR$x$GC_legend$options <- opts

  return(GCVieweR)
}
