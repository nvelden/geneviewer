#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`

#' @import htmlwidgets
#' @export
GCVieweR <- function(data, width = NULL, height = NULL, elementId = NULL){

  # ensure that data is a data frame
  stopifnot(is.data.frame(data))

  # create a list of settings and data to send to the HTML widget
  x <- list()
  x$data <- data

  # Data from functions
  x$addLegend <- list()

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
    padding = list(top = "10px", bottom = "10px", left = "10px", right = "10px")
) {
  # Check if inputs are lists, otherwise make them an empty lists
  # Remove NA or NULL values from the lists
  if (!is.null(padding) && is.list(padding)) {
    padding <- padding[!sapply(padding, function(x) is.na(x) | is.null(x))]
  } else {
    padding <- list()
  }

  if (!is.null(legend) && is.list(legend)) {
    legend <- legend[!sapply(legend, function(x) is.na(x) | is.null(x))]
  } else {
    legend <- list()
  }

  if (!is.null(text) && is.list(text)) {
    text <- text[!sapply(text, function(x) is.na(x) | is.null(x))]
  } else {
    text <- list()
  }

  opts <- list(
    legend = legend,
    text = text,
    orientation = orientation,
    padding = padding,
    backgroundColor = backgroundColor
  )

  # Assuming GCVieweR can pass these options to the drawLegend JavaScript function
  GCVieweR$x$addLegend <- opts
  GCVieweR
}
