#' Add Annotations to a GC_chart
#'
#' This function adds annotations to specified clusters within a GC chart.
#' Annotations can be of various types and are positioned based on provided
#' coordinates. The types of annotations available are: text, textMarker, line,
#' arrow, symbol, rectangle, promoter, and terminator.
#'
#' @param GC_chart A GC chart object to which the annotations will be added.
#' @param cluster Numeric or character vector specifying the clusters to which
#'   annotations should be added.
#' @param type Character vector specifying the type of annotations to add. The
#'   default is "text".
#' @param ... Additional parameters for customization of annotations, depending
#'   on the type.
#'
#' @return Updated GC chart object with added annotations.
#'
#' @examples
#' genes_data <- data.frame(
#'   start = c(10, 50, 90, 130, 170, 210),
#'   end = c(40, 80, 120, 160, 200, 240),
#'   name = c('Gene 1', 'Gene 2', 'Gene 3', 'Gene 4', 'Gene 5', 'Gene 6'),
#'   group = c('A', 'A', 'B', 'B', 'A', 'C'),
#'   cluster = c(1, 1, 1, 2, 2, 2)
#' )
#'
#' # Adding annotations to a GC chart
#' GC_chart(genes_data, cluster = "cluster", group = "group", height = "220px") %>%
#'   GC_annotation(
#'     type = "textMarker",
#'     cluster = 1,
#'     position = 24,
#'     text = "Gene 1",
#'     arrowSize = 8
#'   ) %>%
#'   GC_annotation(
#'     type = "text",
#'     text = "feature 1",
#'     x = 91,
#'     y = 71
#'   ) %>%
#'   GC_annotation(
#'     type = "symbol",
#'     symbol = "triangle",
#'     x = 95,
#'     y = 64,
#'     size = 10,
#'     rotation = 180
#'   ) %>%
#'   GC_annotation(
#'     type = "terminator",
#'     x = 81
#'   ) %>%
#'   GC_annotation(
#'     type = "promoter",
#'     x = 49
#'   ) %>%
#'   # Convenience function to track mouse position on hoover
#'   GC_trackMouse()
#'
#' @seealso \code{\link{GC_trackMouse}}
#'
#' @export
GC_annotation <- function(
    GC_chart,
    type = "textAnnotation",
    cluster = NULL,
    ...
) {

  # Update the GC_chart object with title and options for each cluster
  clusters <- getUpdatedClusters(GC_chart, cluster)

  for(i in seq_along(clusters)){

    # Capture arguments and filter out NULL or empty values
    options <- Filter(function(x) !is.null(x) && length(x) > 0, list(
      type = type,
      ...
    ))

    currentAnnotations <- GC_chart$x$series[[clusters[i]]]$annotations

    if(is.null(currentAnnotations) || length(currentAnnotations) == 0){
      GC_chart$x$series[[clusters[i]]]$annotations <- list(options)
    } else {
      GC_chart$x$series[[clusters[i]]]$annotations <- c(currentAnnotations, list(options))
    }

  }

  return(GC_chart)
}

#' Track Mouse Movement in a GC_chart
#'
#' This function enables or disables mouse tracking on specified clusters within
#' a GC chart. When enabled, the x and y coordinates of the mouse are displayed
#' which can be used to place annotations.
#'
#' @param GC_chart A GC chart object to which the annotations will be added.
#' @param show Logical, specifies whether to track the mouse or not.
#' @param cluster Numeric or character vector specifying the clusters to which
#' annotations should be added.
#' @return Updated GC chart object with mouse tracking settings.
#'
#' @examples
#' genes_data <- data.frame(
#'   start = c(10, 50, 90, 130, 170, 210),
#'   end = c(40, 80, 120, 160, 200, 240),
#'   name = c('Gene 1', 'Gene 2', 'Gene 3', 'Gene 4', 'Gene 5', 'Gene 6'),
#'   group = c('A', 'A', 'B', 'B', 'A', 'C'),
#'   cluster = c(1, 1, 1, 2, 2, 2)
#' )
#'
#' # Enable mouse tracking
#' GC_chart(genes_data, cluster = "cluster", group = "group", height = "220px") %>%
#' GC_trackMouse()
#'
#' @seealso \code{\link{GC_annotation}}
#'
#' @export
GC_trackMouse <- function(
    GC_chart,
    show = TRUE,
    cluster = NULL
) {

  # Update the GC_chart object with title and options for each cluster
  clusters <- getUpdatedClusters(GC_chart, cluster)

  for(i in seq_along(clusters)){

      GC_chart$x$series[[clusters[i]]]$trackMouse$show <- show

  }

  return(GC_chart)
}


#' Set Tooltip for a Gene Chart
#'
#' This function configures the tooltip for a gene chart.
#'
#' @param GC_chart The gene chart object to be modified.
#' @param formatter A character string defining the HTML content of the tooltip. It can
#'   include placeholders like \code{\{start\}} and \code{\{end\}} which will be replaced by actual
#'   data values. The default value shows start and end data.
#' @param show Logical, whether to display the tooltip or not.
#' @param cluster Optional; used to specify which clusters in the chart should have tooltips.
#' @param ... Additional arguments that can be used to further customize the tooltip.
#'
#' @return Returns the gene chart object with the tooltip configured.
#'
#' @examples
#' # Set tooltip
#' genes_data <- data.frame(
#'   start = c(10, 90, 130, 170, 210),
#'   end = c(40, 120, 160, 200, 240),
#'   name = c('Gene 1', 'Gene 2', 'Gene 3', 'Gene 4', 'Gene 5'),
#'   group = c('A', 'B', 'B', 'A', 'C')
#' )
#'
#' # Add tooltips to the gene chart
#' GC_chart(genes_data, group = "group", height = "200px") %>%
#' GC_tooltip(formatter = " <b>Start:</b> {start}<br><b>end:</b> {end}")
#'
#' @export
GC_tooltip <- function(
    GC_chart,
    formatter = "<b>Start:</b> {start}<br><b>End:</b> {end}",
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
      show = recycle_at(show, i)
    )

    # Add arguments to options
    for(name in names(dots)) {
      options[[name]] <- recycle_at(dots[[name]], i)
    }

    # Set title options for each cluster
    GC_chart$x$series[[clusters[i]]]$tooltip <- options

  }
  return(GC_chart)
}
