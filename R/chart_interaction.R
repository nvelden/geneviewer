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
    options <- compact(list(
      type = type,
      ...
    ))

    # A range axis has moved the genes of the cluster; move the annotation along
    options <- shift_annotation(options, GC_chart$x$series[[clusters[i]]]$positionShift)

    currentAnnotations <- GC_chart$x$series[[clusters[i]]]$annotations

    if(is.null(currentAnnotations) || length(currentAnnotations) == 0){
      GC_chart$x$series[[clusters[i]]]$annotations <- list(options)
    } else {
      GC_chart$x$series[[clusters[i]]]$annotations <- c(currentAnnotations, list(options))
    }

  }

  return(GC_chart)
}

#' Move the x positions of an annotation
#'
#' Subtracts `shift` from the x positions of an annotation (`x`, `x1`, `x2`
#' and `position`; for a rectangle the first value of each corner), so it stays
#' on the genes when `GC_scale(axis_type = "range")` has moved them to start at 1.
#' @noRd
shift_annotation <- function(options, shift) {
  if (is.null(shift) || shift == 0) {
    return(options)
  }
  move <- function(x) if (is.numeric(x)) x - shift else x
  for (name in intersect(c("x", "x1", "x2"), names(options))) {
    options[[name]] <- if (is.list(options[[name]])) lapply(options[[name]], move) else move(options[[name]])
  }
  if (!is.null(options$position)) {
    if (identical(options$type, "rectangle")) {
      options$position <- lapply(options$position, function(corners) {
        lapply(corners, function(point) {
          point[1] <- point[1] - shift
          point
        })
      })
    } else if (is.list(options$position)) {
      options$position <- lapply(options$position, move)
    } else {
      options$position <- move(options$position)
    }
  }
  options
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
#' @param ... CSS styles of the coordinate tooltip, such as `backgroundColor`,
#'   `fontSize` or `color`, and `cursor` for the cursor over the cluster
#'   (default "crosshair").
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
    cluster = NULL,
    ...
) {

  # Update the GC_chart object with title and options for each cluster
  clusters <- getUpdatedClusters(GC_chart, cluster)

  for(i in seq_along(clusters)){

      GC_chart$x$series[[clusters[i]]]$trackMouse <- c(list(show = recycle_at(show, i)), list(...))

  }

  return(GC_chart)
}

#' Report Chart Interactions and Completed Updates
#'
#' Makes a chart report clicks to Shiny, so an app can react to the part of the
#' chart that was clicked: show the details of a gene, open the settings of the
#' clicked part, and so on. Charts are not clickable unless this is set, and
#' click reporting has no effect outside Shiny.
#'
#' Each click sets `input$<outputId>_click`, e.g. `input$chart_click` for
#' `GC_chartOutput("chart")`, to a list with:
#' \describe{
#'   \item{component}{The part of the chart that was clicked: `"genes"`,
#'     `"transcript"`, `"labels"`, `"coordinates"`, `"links"`, `"legend"`,
#'     `"scale"`, `"scaleBar"`, `"sequence"`, `"title"` (the chart title),
#'     `"clusterTitle"`, `"clusterFooter"`, `"clusterLabel"` or
#'     `"annotations"`.}
#'   \item{cluster}{The cluster that was clicked, or `NULL` for the chart title,
#'     the legend and links between clusters.}
#'   \item{data}{The data of the clicked element, or `NULL`: the row of the
#'     gene for genes, transcripts and labels, the row of the link for links,
#'     and the group for the legend.}
#' }
#'
#' Every part of the chart is marked with a `data-component` attribute and each
#' cluster with `data-cluster`, also without this function, so they can be
#' selected with CSS or JavaScript.
#'
#' With `dblclick = TRUE`, double-clicks independently report the same payload
#' to `input$<outputId>_dblclick` and the bubbling browser event
#' `geneviewer:dblclick`. Legend double-clicks are excluded, so visibility
#' toggles do not open an editor. Browsers also send clicks during a
#' double-click: use clicks for selection, and double-clicks for editing,
#' rather than opening different settings panels on each event.
#'
#' With `select = TRUE`, a click selects an individual gene or label and
#' outlines it. Clicking empty chart space or any other part of the chart, or
#' pressing Escape, clears selection.
#' Selection reports the click payload (or `NULL` when cleared) to
#' `input$<outputId>_select` and `geneviewer:select`. It survives redraws of the
#' same data and resize. Selected items can be opened with Enter when
#' `dblclick = TRUE`; items are keyboard focusable in selection mode.
#' Use `click = FALSE` to avoid legacy single-click settings handlers.
#' `legend_click = FALSE` excludes legend clicks from click reporting without
#' changing legend visibility toggles. Selection never includes the legend.
#' Double-click and selection browser events also work outside Shiny.
#'
#' With `rendered = TRUE`, each successful update also sets
#' `input$<outputId>_rendered` to a list containing `id` (the supplied
#' `render_id`, or a generated ID), `chartId` (the chart's DOM element ID),
#' `width` and `height` (the widget size in pixels). When `render_id` is `NULL`,
#' a new ID is generated in the browser for each successful update, unique
#' across charts on the page. `chartId` stays the same when that chart updates;
#' generated render IDs change. Explicit IDs, including `0` and `""`, are kept.
#' It fires after the chart, legend, links and click handlers have been built,
#' once per data update, including repeated identifiers, but not on resize or
#' a failed draw. It does not wait for browser paint or external image loading.
#'
#' The widget also dispatches a bubbling `geneviewer:rendered` browser event
#' with the same fields in `event.detail`, including outside Shiny. A loading
#' indicator can listen to it without inspecting the widget payload. Use a
#' different `render_id` for each load and ignore events for older loads.
#'
#' @param GC_chart A GC chart object.
#' @param click Logical, whether to report clicks to Shiny.
#' @param highlight Logical, whether to outline the part of the chart under the
#'   mouse, to show what a click selects.
#' @param rendered Logical, whether to report successful chart updates.
#' @param render_id Optional single character string or finite number returned
#'   with the rendered event, to identify the update. The default, `NULL`,
#'   generates a unique ID for each update in the browser. It is not a DOM
#'   element ID; the event's `chartId` identifies the chart itself.
#' @param dblclick Logical, whether to report double-clicks for editing.
#' @param select Logical, whether to enable gene and label selection.
#' @param legend_click Logical, whether legend clicks are reported to Shiny.
#'
#' @return The GC chart object with the event settings.
#'
#' @examples
#' genes_data <- data.frame(
#'   start = c(10, 50, 90, 130),
#'   end = c(40, 80, 120, 160),
#'   name = c("Gene 1", "Gene 2", "Gene 3", "Gene 4"),
#'   group = c("A", "A", "B", "C")
#' )
#'
#' if (interactive()) {
#'   library(shiny)
#'   ui <- fluidPage(
#'     GC_chartOutput("chart", height = "200px"),
#'     verbatimTextOutput("clicked")
#'   )
#'   server <- function(input, output, session) {
#'     output$chart <- renderGC_chart(
#'       GC_chart(genes_data, group = "group") %>%
#'         GC_labels("name") %>%
#'         GC_events(highlight = TRUE)
#'     )
#'     output$clicked <- renderPrint(str(input$chart_click))
#'   }
#'   shinyApp(ui, server)
#' }
#'
#' @export
GC_events <- function(GC_chart, click = TRUE, highlight = FALSE,
                      rendered = FALSE, render_id = NULL, dblclick = FALSE,
                      select = FALSE, legend_click = TRUE) {
  for (arg in list(click, highlight, rendered, dblclick, select, legend_click)) {
    if (!is.logical(arg) || length(arg) != 1 || is.na(arg)) {
      stop("Event options must be TRUE or FALSE.")
    }
  }
  if (!is.null(render_id) &&
      (length(render_id) != 1 || is.na(render_id) ||
       !(is.character(render_id) || (is.numeric(render_id) && !is.complex(render_id) &&
                                     is.finite(render_id))))) {
    stop("'render_id' must be NULL, a single character string or a finite number.")
  }

  GC_chart$x$events <- list(click = click, highlight = highlight)
  if (dblclick) GC_chart$x$events$dblclick <- TRUE
  if (select) GC_chart$x$events$select <- TRUE
  if (!legend_click) GC_chart$x$events$legendClick <- FALSE
  if (rendered) {
    GC_chart$x$events$rendered <- TRUE
    GC_chart$x$events$renderId <- render_id
  }
  GC_chart
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

  # Set tooltip options for each cluster
  set_cluster_options(GC_chart, cluster, "tooltip", function(i, series) {
    # Default options
    options <- list(
      formatter = formatter,
      show = recycle_at(show, i)
    )

    # Add arguments to options
    recycle_dots(options, dots, i)
  })
}
