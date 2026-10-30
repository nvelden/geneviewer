#' Add Annotations to a GC_chart
#'
#' This function adds annotations to specified clusters within a GC chart.
#' Annotations can be of various types and are positioned based on provided
#' coordinates. The types of annotations available are: text, textMarker, line,
#' arrow, symbol, rectangle, promoter, terminator and arc.
#'
#' @param GC_chart A GC chart object to which the annotations will be added.
#' @param cluster Numeric or character vector specifying the clusters to which
#'   annotations should be added.
#' @param type The type of annotation to add: one of "text" (default),
#'   "textMarker", "line", "arrow", "symbol", "rectangle", "promoter",
#'   "terminator" or "arc".
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
    type = "text",
    cluster = NULL,
    ...
) {

  if (!is.character(type) || length(type) != 1 || !type %in% annotation_types) {
    stop("'type' must be one of ", quoted(annotation_types), ".")
  }

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

# The annotation types, as drawn by createAnnotation() in geneviewer.js
annotation_types <- c("text", "textMarker", "line", "arrow", "symbol",
                      "rectangle", "promoter", "terminator", "arc")

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
#' `geneviewer:dblclick`. Browsers also send clicks during a
#' double-click: use clicks for selection, and double-clicks for editing,
#' rather than opening different settings panels on each event.
#'
#' With `select = TRUE`, a click selects an individual editable element and
#' outlines it. Clicking it again, empty chart space or a noneditable part of
#' the chart, or pressing Escape, clears selection. Shift-, Ctrl- or
#' Cmd-click adds an element to the selection or takes it out, and dragging on
#' empty chart space draws a box that selects the elements entirely inside it
#' (with Shift, Ctrl or Cmd, in addition to the selection). With `drag = TRUE`
#' as well, dragging one of the selected elements moves all of them that can
#' be moved, and the arrow keys move them together; such a move of several
#' elements is one step to undo, reported as `input$<outputId>_moves`.
#' Selection reports the payload of the element selected last (or `NULL` when
#' cleared) to `input$<outputId>_select` and `geneviewer:select`, and a list of
#' the payloads of all selected elements, in the order they were selected, to
#' `input$<outputId>_selection` and `geneviewer:selection`. It survives redraws of the
#' same data and resize. When a redraw changes the selected element's style,
#' its updated payload is reported again. Selected items can be opened with Enter when
#' `dblclick = TRUE`; items are keyboard focusable in selection mode.
#' Use `click = FALSE` to avoid legacy single-click settings handlers.
#' `legend_click = FALSE` excludes legend clicks from click reporting without
#' enabling legend click editing. Legend labels and patches are individually
#' selectable, with `part` set to `"label"` or `"marker"` and `data` set to
#' the original group value. Legend clicks edit instead of toggling visibility
#' when selection or double-click events are enabled, or when click reporting
#' is enabled with `legend_click = TRUE`. Otherwise they hide/show genes.
#' Double-click and selection browser events also work outside Shiny.
#'
#' Click, double-click and selection payloads of editable elements also contain
#' `style`: how it is drawn, with the names of [GC_element_style()] options, so
#' an editor can show the current values. For a gene these are `fill`,
#' `opacity`, `stroke`, `strokeWidth`, `strokeDasharray`, `marker`,
#' `markerSize`, `markerHeight`, `arrowheadWidth`, `arrowheadHeight`,
#' `cornerRadius` and `y`; for a label `fill`, `opacity`, `text`, `fontSize`
#' (in pixels), `fontWeight`, `fontStyle` and `fontFamily`. Colours are hex
#' codes, except transparent colours, `"none"` and patterns. Legend labels
#' report the same text options; legend markers report `fill`, `opacity`,
#' `stroke`, `strokeWidth` and `strokeDasharray`.
#'
#' With `drag = TRUE`, the undo, redo and reset icons next to the download
#' icon undo and redo moves, and move every element back to where it was first
#' drawn, so positions set with [GC_element_style()] are kept. They show while
#' the mouse is over the chart, once there is a move. After clicking the
#' chart, Ctrl+Z (Cmd+Z on a Mac) undoes and Ctrl+Shift+Z or Ctrl+Y redoes.
#' With `select = TRUE` as well, the arrow keys move the selected text or
#' scale by a pixel, or 10 pixels with Shift. Each drag is one step to undo,
#' arrow-key moves of one element within a second are one step, and a reset
#' is one step. A step that moves one element reports it like a drag, with
#' `action` set to `"undo"`, `"redo"` or `"nudge"`. A step that moves more,
#' such as a reset, reports `input$<outputId>_moves` and `geneviewer:moves`
#' once, with `action` (`"reset"`, `"undo"` or `"redo"`) and `moves`, a list
#' of drag payloads, so an app that keeps drags keeps these too. Steps whose
#' element was since moved by something else are dropped. Outside Shiny,
#' moves are kept when the chart is resized.
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
#' @param select Logical, whether to enable individual element selection.
#' @param drag Logical, enable dragging gene labels, chart titles and cluster
#'   titles, footers, labels, whole scale bars and axes, and legend labels and
#'   markers, each on its own. Off by default. Releasing a drag reports
#'   `input$<outputId>_drag` and `geneviewer:drag`, with `position` containing
#'   `moveX` and `moveY` offsets in whole SVG pixels and `action` set to `"drag"`.
#'   Apply them with [GC_element_style()] to preserve the position on redraw.
#'   Escape cancels an active drag. Moves can be undone and redone, see Details.
#' @param legend_click Logical, whether legend clicks are reported to Shiny.
#' @param zoom Logical, whether to report the window of the zoom slider of
#'   [GC_zoom()] as `input$<outputId>_zoom` and `geneviewer:zoom`: `mode`,
#'   `start` and `end` of the window in the units of the mode (whole base
#'   pairs, or shares to 0.0001), `zoomed` (`FALSE` when everything shows),
#'   `range`, the window to pass to `GC_zoom(range = )` (`NULL` when
#'   everything shows), `clusters`, the cluster and the first and last base
#'   pair it shows, and `action`: `"drag"`, `"keyboard"` or `"reset"` by the
#'   user, `"set"` by [GC_zoom_to()], or `"render"` when a render changed the
#'   window, e.g. new data. It is reported on the first render and after
#'   every change, so an app always knows the window.
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
                      select = FALSE, legend_click = TRUE, drag = FALSE, zoom = FALSE) {
  for (arg in list(click, highlight, rendered, dblclick, select, legend_click, drag, zoom)) {
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
  if (drag) {
    GC_chart$x$events$drag <- TRUE
    # The undo and redo icons, like the save icon
    GC_chart <- add_fontawesome(GC_chart)
  }
  if (!legend_click) GC_chart$x$events$legendClick <- FALSE
  if (zoom) GC_chart$x$events$zoom <- TRUE
  if (rendered) {
    GC_chart$x$events$rendered <- TRUE
    GC_chart$x$events$renderId <- render_id
  }
  GC_chart
}


#' Set the Toolbar of a GC Chart
#'
#' The toolbar holds the chart's own controls, in a corner of the chart: undo,
#' redo and reset of moves, with [GC_events()]`(drag = TRUE)`, and the save
#' button of [GC_chart()]`(save_button = TRUE)`. `GC_toolbar()` sets when and
#' where it shows, which buttons it has, its colours and size, and the icons
#' of its buttons. Colours and size can also be set for all charts of a theme,
#' with [GC_theme()]`(toolbar = list(...))`; `GC_toolbar()` takes precedence.
#'
#' Icons are [Font Awesome](https://fontawesome.com/search?ic=free) icons,
#' by name, e.g. `"arrow-rotate-left"` for a solid icon, or by their classes,
#' e.g. `"fa-regular fa-floppy-disk"` for another style.
#'
#' @param GC_chart A GC chart object.
#' @param show When the toolbar shows: `"hover"` (default) while the mouse is
#'   over the chart or one of its buttons has keyboard focus, `"always"` (e.g.
#'   on touch screens), or `"never"`. Keyboard shortcuts work also when it
#'   does not show.
#' @param position The corner of the chart: `"topright"` (default),
#'   `"topleft"`, `"bottomright"` or `"bottomleft"`.
#' @param buttons The buttons, of `"undo"`, `"redo"`, `"reset"`, `"zoom"` and
#'   `"save"` (default: all). Undo, redo and reset show with
#'   [GC_events()]`(drag = TRUE)`, the zoom button (reset zoom) with [GC_zoom()]
#'   while zoomed in, the save button unless [GC_chart()]`(save_button =
#'   FALSE)`.
#' @param size Size of the icons in pixels (default 20).
#' @param color Colour of the icons (default `"#D3D3D3"`).
#' @param hoverColor Colour of an icon under the mouse (default `"#555555"`).
#' @param tooltipBackground Background colour of the tooltips and of the
#'   menu of the save button (default `"#333333"`).
#' @param tooltipColor Text colour of the tooltips and the menu (default
#'   `"white"`).
#' @param tooltipBorder CSS border of the tooltips and the menu, e.g.
#'   `"1px solid #b7b9be"` (default none).
#' @param tooltipRadius Corner radius of the tooltips and the menu: pixels,
#'   or a CSS length (default 3).
#' @param tooltipShadow CSS box shadow of the tooltips and the menu, e.g.
#'   `"1px 2px 10px rgba(0, 0, 0, 0.2)"` (default none).
#' @param fontFamily Font of the tooltips and the menu (default Arial).
#' @param icons Icons of the buttons: a named list or character vector, with
#'   names of `buttons`, e.g. `list(reset = "arrows-to-circle", save =
#'   "camera")`. Buttons that are not named keep their icon.
#'
#' @return The GC chart object with the toolbar settings.
#'
#' @examples
#' genes_data <- data.frame(
#'   start = c(10, 50, 90),
#'   end = c(40, 80, 120),
#'   name = c("Gene 1", "Gene 2", "Gene 3")
#' )
#' GC_chart(genes_data, height = "150px") %>%
#'   GC_labels("name") %>%
#'   GC_events(select = TRUE, drag = TRUE) %>%
#'   GC_toolbar(show = "always", position = "topleft", size = 16,
#'              color = "#9e9e9e", hoverColor = "black",
#'              icons = list(reset = "arrows-to-circle", save = "camera"))
#'
#' @export
GC_toolbar <- function(GC_chart, show = NULL, position = NULL, buttons = NULL, size = NULL,
                       color = NULL, hoverColor = NULL, tooltipBackground = NULL,
                       tooltipColor = NULL, tooltipBorder = NULL, tooltipRadius = NULL,
                       tooltipShadow = NULL, fontFamily = NULL, icons = NULL) {
  options <- Filter(Negate(is.null), list(
    show = show, position = position, buttons = buttons, size = size, color = color,
    hoverColor = hoverColor, tooltipBackground = tooltipBackground,
    tooltipColor = tooltipColor, tooltipBorder = tooltipBorder, tooltipRadius = tooltipRadius,
    tooltipShadow = tooltipShadow, fontFamily = fontFamily, icons = icons))
  check_toolbar(options)
  if (!is.null(options$buttons)) options$buttons <- as.list(options$buttons)
  if (!is.null(options$icons)) options$icons <- as.list(options$icons)
  GC_chart$x$toolbar <- utils::modifyList(if (is.null(GC_chart$x$toolbar)) list() else GC_chart$x$toolbar,
                                          options)
  # Custom icons and the undo and redo icons are Font Awesome icons
  GC_chart <- add_fontawesome(GC_chart)
  GC_chart
}

# The buttons of the toolbar, in their order (also in geneviewerwidget.js)
toolbar_buttons <- c("undo", "redo", "reset", "zoom", "save")

# The Font Awesome stylesheet, once, for the save, undo and redo icons
add_fontawesome <- function(GC_chart) {
  fontawesome <- fontawesome::fa_html_dependency()
  known <- vapply(GC_chart$dependencies, function(dep) identical(dep$name, fontawesome$name), logical(1))
  if (!any(known)) GC_chart$dependencies <- c(GC_chart$dependencies, list(fontawesome))
  GC_chart
}

# Choices for error messages: "a", "b", "c"
quoted <- function(x) paste0('"', x, '"', collapse = ", ")

check_toolbar <- function(options) {
  one_of <- function(name, choices) {
    value <- options[[name]]
    if (!is.null(value) && !(is.character(value) && length(value) == 1 && value %in% choices)) {
      stop("'", name, "' must be one of ", quoted(choices), ".")
    }
  }
  one_of("show", c("hover", "always", "never"))
  one_of("position", c("topright", "topleft", "bottomright", "bottomleft"))
  if (!is.null(options$buttons) && (!is.character(options$buttons) ||
                                    !all(options$buttons %in% toolbar_buttons))) {
    stop("'buttons' must be of ", quoted(toolbar_buttons), ".")
  }
  if (!is.null(options$size) && (!is.numeric(options$size) || length(options$size) != 1 ||
                                 is.na(options$size) || options$size <= 0)) {
    stop("'size' must be a single positive number of pixels.")
  }
  colours <- c("color", "hoverColor", "tooltipBackground", "tooltipColor")
  for (name in c(colours, "tooltipBorder", "tooltipShadow", "fontFamily")) {
    value <- options[[name]]
    if (!is.null(value) && !(is.character(value) && length(value) == 1 && !is.na(value))) {
      stop("'", name, "' must be a single ", if (name %in% colours) "colour" else "character string", ".")
    }
  }
  radius <- options$tooltipRadius
  if (!is.null(radius) && !(length(radius) == 1 && !is.na(radius) &&
                            (is.character(radius) || (is.numeric(radius) && radius >= 0)))) {
    stop("'tooltipRadius' must be a number of pixels or a CSS length.")
  }
  icons <- options$icons
  if (!is.null(icons)) {
    if (is.list(icons)) icons <- unlist(icons)
    if (!is.character(icons) || is.null(names(icons)) || !all(names(icons) %in% toolbar_buttons) ||
        anyNA(icons) || any(!nzchar(icons))) {
      stop("'icons' must name Font Awesome icons for ", quoted(toolbar_buttons),
           ", e.g. list(reset = \"arrows-to-circle\").")
    }
  }
  invisible()
}


#' Add a Zoom Slider to a GC Chart
#'
#' `GC_zoom()` adds a slider below the clusters to zoom in on part of them.
#' Drag a handle to change the window, drag the window to move it, click the
#' track to move the window there, and double-click the slider or use the
#' reset zoom button of the toolbar ([GC_toolbar()]) to show everything again.
#' With the keyboard, the arrow keys move a handle or the window by 1% of the
#' slider (Shift: 10%), Home and End to the ends.
#'
#' The chart is drawn again for each window: genes, labels, links,
#' coordinates and annotations follow the window, and parts outside it are
#' cut off at the edges of the plot area. The axis shows the ticks of the
#' window. The window of [GC_scale()]`(start, end)` is the whole range the
#' slider covers. Text moved by dragging ([GC_events()]`(drag = TRUE)`) keeps
#' its offset. Saved images leave out the slider and the room it takes.
#'
#' In Shiny, [GC_events()]`(zoom = TRUE)` reports the window, and
#' [GC_zoom_to()] sets it from the server.
#'
#' @param GC_chart A GC chart object.
#' @param show Logical, whether to show the slider. `FALSE` removes it.
#' @param range The window shown first, as `c(start, end)` in the units of
#'   `mode`. `NULL` (default) shows everything. A Shiny app can keep the
#'   window the user picked with `range = input$<outputId>_zoom$range` (see
#'   [GC_events()]`(zoom = TRUE)`). The chart keeps the window the user picked
#'   when it is drawn again with the same data, the same `range` and `mode`
#'   and the same [GC_events()]`(render_id = )`; a new `render_id` marks a new
#'   load, which starts at `range` again.
#' @param mode How the window applies to the clusters: `"relative"` (default)
#'   zooms every cluster to the same share of its own range, e.g.
#'   `range = c(0.25, 0.5)` for its second quarter, which suits clusters of
#'   different genomes. `"absolute"` shows the same base pairs in every
#'   cluster, e.g. `range = c(12000, 18000)`, and the slider spans the range
#'   of all clusters.
#' @param min The narrowest window, in the units of `mode` (default 1% of the
#'   slider).
#' @param update `"live"` (default) draws the chart while the slider moves,
#'   `"release"` when it is let go, for large charts.
#' @param height Height of the slider in pixels (default 24). The clusters
#'   make room for it.
#' @param overview Logical, whether the slider shows the genes of each
#'   cluster, one row per cluster, in the colours of their group (default
#'   `TRUE`).
#' @param color One colour for the genes of the overview instead of their
#'   group colours.
#' @param style,windowStyle,handleStyle Named lists of CSS styles of the
#'   slider track, the window and the two handles, e.g.
#'   `style = list(backgroundColor = "#eeeeee")`.
#'
#' @return The GC chart object with the zoom slider.
#'
#' @examples
#' GC_chart(ophA_clusters, cluster = "cluster", group = "class",
#'          height = "300px") %>%
#'   GC_labels("name") %>%
#'   GC_zoom(range = c(0.2, 0.6))
#'
#' # The same base pairs in every cluster
#' GC_chart(ophA_clusters, cluster = "cluster", group = "class",
#'          height = "300px") %>%
#'   GC_zoom(mode = "absolute", update = "release", height = 30,
#'           handleStyle = list(backgroundColor = "#d9d9d9"))
#'
#' @seealso [GC_zoom_to()], [GC_events()], [GC_scale()]
#' @export
GC_zoom <- function(GC_chart, show = TRUE, range = NULL, mode = NULL, min = NULL,
                    update = NULL, height = NULL, overview = NULL, color = NULL,
                    style = NULL, windowStyle = NULL, handleStyle = NULL) {
  # A range from Shiny, e.g. input$chart_zoom$range, arrives as a list
  if (is.list(range)) range <- unlist(range)
  options <- Filter(Negate(is.null), list(
    show = show, range = range, mode = mode, min = min, update = update,
    height = height, overview = overview, color = color, style = style,
    windowStyle = windowStyle, handleStyle = handleStyle))
  check_zoom(options)
  old <- if (is.null(GC_chart$x$zoom)) list() else GC_chart$x$zoom
  # A new mode does not keep a range or minimum of the old one
  if (!is.null(options$mode) && !identical(options$mode, old$mode)) old$range <- old$min <- NULL
  if (!is.null(options$range)) options$range <- as.numeric(sort(options$range))
  GC_chart$x$zoom <- utils::modifyList(old, options)
  # The icon of the reset zoom button
  add_fontawesome(GC_chart)
}

check_zoom <- function(options) {
  flag <- function(name) {
    value <- options[[name]]
    if (!is.null(value) && !(is.logical(value) && length(value) == 1 && !is.na(value))) {
      stop("'", name, "' must be TRUE or FALSE.")
    }
  }
  flag("show")
  flag("overview")
  one_of <- function(name, choices) {
    value <- options[[name]]
    if (!is.null(value) && !(is.character(value) && length(value) == 1 && value %in% choices)) {
      stop("'", name, "' must be one of ", quoted(choices), ".")
    }
  }
  one_of("mode", c("relative", "absolute"))
  one_of("update", c("live", "release"))
  range <- options$range
  if (!is.null(range)) {
    if (!is.numeric(range) || length(range) != 2 || anyNA(range) || any(!is.finite(range)) ||
        range[1] == range[2]) {
      stop("'range' must be two different numbers: c(start, end).")
    }
    if (!identical(options$mode, "absolute") && (min(range) < 0 || max(range) > 1)) {
      stop("A relative 'range' is shares of the clusters, between 0 and 1, e.g. c(0.25, 0.5). ",
           "Use mode = \"absolute\" for base pairs.")
    }
  }
  for (name in c("min", "height")) {
    value <- options[[name]]
    if (!is.null(value) && !(is.numeric(value) && length(value) == 1 && !is.na(value) && value > 0)) {
      stop("'", name, "' must be a single positive number.")
    }
  }
  if (!is.null(options$min) && !identical(options$mode, "absolute") && options$min > 1) {
    stop("A relative 'min' is a share of the slider, between 0 and 1.")
  }
  color <- options$color
  if (!is.null(color) && !(is.character(color) && length(color) == 1 && !is.na(color))) {
    stop("'color' must be a single colour.")
  }
  for (name in c("style", "windowStyle", "handleStyle")) {
    value <- options[[name]]
    if (!is.null(value) && !(is.list(value) && (length(value) == 0 || !is.null(names(value))))) {
      stop("'", name, "' must be a named list of CSS styles.")
    }
  }
  invisible()
}

#' Set the Zoom of a GC Chart in Shiny
#'
#' Moves the zoom slider of [GC_zoom()] in a running Shiny app, without
#' drawing the chart again from R.
#'
#' @param outputId The output ID of the chart, as in [GC_chartOutput()].
#' @param range The window, as `c(start, end)` in the units of the `mode` of
#'   [GC_zoom()]: shares of the clusters (`"relative"`) or base pairs
#'   (`"absolute"`). `NULL` shows everything.
#' @param session The Shiny session.
#'
#' @return Nothing; called for its effect.
#'
#' @examples
#' if (interactive()) {
#'   library(shiny)
#'   ui <- fluidPage(
#'     actionButton("first", "First half"),
#'     actionButton("all", "Everything"),
#'     GC_chartOutput("chart", height = "300px")
#'   )
#'   server <- function(input, output, session) {
#'     output$chart <- renderGC_chart(
#'       GC_chart(ophA_clusters, cluster = "cluster", group = "class") %>%
#'         GC_zoom() %>%
#'         GC_events(zoom = TRUE)
#'     )
#'     observeEvent(input$first, GC_zoom_to("chart", c(0, 0.5)))
#'     observeEvent(input$all, GC_zoom_to("chart", NULL))
#'     observeEvent(input$chart_zoom, str(input$chart_zoom))
#'   }
#'   shinyApp(ui, server)
#' }
#'
#' @seealso [GC_zoom()]
#' @export
GC_zoom_to <- function(outputId, range = NULL, session = shiny::getDefaultReactiveDomain()) {
  if (is.list(range)) range <- unlist(range)
  if (!is.character(outputId) || length(outputId) != 1 || is.na(outputId)) {
    stop("'outputId' must be the output ID of the chart.")
  }
  if (!is.null(range) && (!is.numeric(range) || length(range) != 2 || anyNA(range) ||
                          any(!is.finite(range)))) {
    stop("'range' must be NULL or two numbers: c(start, end).")
  }
  if (is.null(session)) stop("GC_zoom_to() works in a Shiny session.")
  session$sendCustomMessage("geneviewer-zoom", list(
    id = session$ns(outputId),
    range = if (is.null(range)) NULL else as.list(as.numeric(sort(range)))))
  invisible()
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
