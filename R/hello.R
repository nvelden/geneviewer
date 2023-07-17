#' @import htmlwidgets
#' @export
GCVieweR <- function(data, width = NULL, height = NULL, elementId = NULL){

  # ensure that data is a data frame
  stopifnot(is.data.frame(data))

  # create a list of settings and data to send to the HTML widget
  x <- list()
  x$data <- data
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
