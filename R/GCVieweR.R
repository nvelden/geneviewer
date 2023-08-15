#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`

#' @import htmlwidgets
#' @export
GCVieweR <- function(data, start = start, stop = stop, group = NULL,
                     width = "100%", height = NULL, elementId = NULL){

  # ensure that data is a data frame
  stopifnot(is.data.frame(data))

  # use rlang to capture the column names
  start_col <- rlang::enquo(start)
  stop_col <- rlang::enquo(stop)
  group_col <- if(!is.null(group)) rlang::enquo(group) else NULL

  # Check if column names are in the data frame
  colnames_data <- colnames(data)
  if (!(rlang::quo_name(start_col) %in% colnames_data)) stop("start column not found in data")
  if (!(rlang::quo_name(stop_col) %in% colnames_data)) stop("stop column not found in data")
  if (!is.null(group_col) && !(rlang::quo_name(group_col) %in% colnames_data)) stop("group column not found in data")

  # create a list of settings and data to send to the HTML widget
  x <- list()
  x$data <- data
  x$data$start <- rlang::eval_tidy(start_col, data = data)
  x$data$stop <- rlang::eval_tidy(stop_col, data = data)
  x$data$group <- if(!is.null(group_col)) rlang::eval_tidy(group_col, data = data) else NULL

  # Data from functions
  x$GC_legend <- list()
  x$GC_title <- list()
  x$GC_genes <- list()
  x$GC_labels <- list()
  x$GC_cluster <- list()
  x$GC_coordinates <- list()
  x$GC_scaleBar <- list()
  x$GC_footer <- list()
  x$GC_clusterLabel <- list()

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
GC_scaleBar <- function(
    GCVieweR,
    scaleBar = TRUE,
    title = "1 kb",
    scaleBarUnit = 1000,
    options = list()
) {

  # Default options
  defaultOptions <- list(
    scaleBar = scaleBar,
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
    position = "left",
    xOffset = 0,
    yOffset = 0,
    wrapLabel = TRUE,
    options = list()
) {

  # Default font options
  defaultOptions <- list(
    title = title,
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
    font = list(
      size = "12px",
      style = "italic",
      weight = "normal",
      decoration = "none",
      family = "sans-serif",
      color = "black"
    ),
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
    font = font,
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
    coordinates = TRUE,
    options = list()
) {

  # Default options
  defaultOptions <- list(
    coordinates = coordinates
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
    color,
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

  color <- deparse(substitute(color))

  # Default options
  defaultOptions <- list(
    color = color,
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
    spacing = 10,
    position = "center",
    xOffset = 10,
    yOffset = 0,
    options = list()
) {

  # Default options
  defaultOptions <- list(
    title = title,
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
    color,
    position = "top",
    orientation = "horizontal",
    height = "10%",
    xOffset = 10,
    yOffset = 10,
    margin = list(top = 0, right = 0, bottom = 0, left = 0),
    backgroundColor = "#FFF",
    options = list()
) {

  color <- deparse(substitute(color))

  # Define default options
  defaultOptions <- list(
    x = xOffset,
    y = yOffset,
    color = color,
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

#' @export
GC_cluster <- function(
    GCVieweR,
    start = start,
    stop = stop,
    cluster = cluster
) {
  padding <- cleanList(padding)
  legend <- cleanList(legend)
  text <- cleanList(text)

  opts <- list(
    legend = legend,
    text = text,
    orientation = orientation,
    position = position,
    padding = padding,
    backgroundColor = backgroundColor
  )

  GCVieweR$x$GC_cluster <- opts
  GCVieweR
}
