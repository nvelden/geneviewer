#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`

#' @import htmlwidgets
#' @export
#' @import htmlwidgets
#' @export
GCVieweR <- function(data, start = start, stop = stop, group = NULL, width = NULL, height = NULL, elementId = NULL){

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
  x$addLegend <- list()
  x$addCluster <- list()

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
addLegend <- function(
    GCVieweR,
    legend = list(size = 10, padding = 5),
    text = list(size = 10),
    orientation = "horizontal",
    backgroundColor = "#FFF",
    position = "top",
    padding = list(top = 10, bottom = 10, left = 10, right = 10)
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

  GCVieweR$x$addLegend <- opts
  GCVieweR
}

#' @export
addCluster <- function(
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

  GCVieweR$x$addLegend <- opts
  GCVieweR
}
