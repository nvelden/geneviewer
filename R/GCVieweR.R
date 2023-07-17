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
