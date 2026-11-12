#' Add Title to a GC_chart
#'
#' Modify the cluster title and subtitle of specified clusters within a GC chart and adjust
#' the display settings.
#'
#' @param GC_chart A GC chart object.
#' @param title Character vector. Titles to set for the clusters.
#' @param subtitle Character vector. Subtitles to set for the clusters.
#' @param show Logical. Whether to display the title. Default is TRUE.
#' @param height Character. Height for the title (e.g., "50px"). Default is
#'   NULL: the height of the theme, otherwise 50 pixels with a subtitle and 40
#'   without.
#' @param style A list of CSS styles to be applied to the chart container. Each
#'   element of the list should be a valid CSS property-value pair. For example,
#'   list(backgroundColor = "white", border = "2px solid black").
#'   Default is an empty list.
#' @param titleFont List. Settings for the title font.
#' @param subtitleFont List. Settings for the subtitle font.
#' @param align Character. Horizontal alignment of the title and subtitle,
#'   "left", "center" or "right". `x` moves them from this position. Default is
#'   NULL: the alignment of the theme, otherwise "center".
#' @param spacing Numeric. Pixels between the title and the subtitle. Default
#'   is NULL: as set by the theme, otherwise 20.
#' @param ... Additional customization arguments for title and subtitle.
#'
#' @return Updated GC chart with new title settings.
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
#' # Basic usage
#' GC_chart(genes_data, cluster ="cluster", group = "group", height = "400px") %>%
#' GC_labels("name") %>%
#' GC_title(
#'   title = "Cluster 1 Data",
#'   subtitle = "Detailed View",
#'   show = TRUE
#' )
#'
#' # Customizing title style
#' GC_chart(genes_data, cluster = "cluster", group = "group", height = "500px") %>%
#'   GC_labels("name") %>%
#'   GC_title(
#'     title = "Cluster 1 Data",
#'     subtitle = "Detailed View",
#'     show = TRUE,
#'     height = "50px",
#'     cluster = 1,
#'     x = 0,
#'     y = 25, # height / 2
#'     align = "center",
#'     spacing = 20,
#'     style = list(
#'       backgroundColor = "#0000"
#'       # Any other CSS styles
#'     ),
#'     titleFont = list(
#'       fontSize = "16px",
#'       fontStyle = "normal",
#'       fontWeight = "bold",
#'       textDecoration = "normal",
#'       fontFamily = "sans-serif",
#'       cursor = "default",
#'       fill = "black"
#'       # Any other CSS styles
#'     ),
#'     subtitleFont = list(
#'       fontSize = "14px",
#'       fontStyle = "normal",
#'       fontWeight = "bold",
#'       textDecoration = "normal",
#'       fontFamily = "sans-serif",
#'       cursor = "default",
#'       fill = "black"
#'       # Any other CSS styles
#'     )
#'   )
#'
#' @export
GC_title <- function(
    GC_chart,
    title = NULL,
    subtitle = NULL,
    style = list(),
    subtitleFont = list(),
    titleFont = list(),
    show = TRUE,
    height = NULL,
    align = NULL,
    spacing = NULL,
    ...
) {

  spec <- spec_args(match.call(), environment())
  if (!is.logical(show) || length(show) != 1) {
    stop("show must be a single TRUE or FALSE")
  }
  check_align(align)
  check_spacing(spacing)

  # A call without a title changes the title set before, e.g. show = FALSE
  # hides it; on a chart without a title it changes nothing
  if (is.null(GC_chart$x$spec[["title"]]) && (!show || is.null(title) && is.null(subtitle))) {
    return(GC_chart)
  }

  add_spec(GC_chart, "title", spec, mode = "merge")
}

#' Update cluster Title of a GC Chart Cluster
#'
#' Modify the cluster title and subtitle of specified clusters within a GC chart and adjust
#' the display settings.
#'
#' @param GC_chart A GC chart object.
#' @param title Character vector. Titles to set for the clusters.
#' @param subtitle Character vector. Subtitles to set for the clusters.
#' @param show Logical. Whether to display the title. Default is TRUE.
#' @param height Character. Height for the title (e.g., "40px").
#' @param cluster Numeric or character vector. Clusters in the GC chart to update.
#' @param titleFont List. Settings for the title font.
#' @param subtitleFont List. Settings for the subtitle font.
#' @param align Character vector. Horizontal alignment of the title and
#'   subtitle, "left", "center" or "right", recycled over the clusters. `x`
#'   moves them from this position. Default is NULL: the alignment of the
#'   theme, otherwise "center".
#' @param spacing Numeric vector. Pixels between the title and the subtitle,
#'   recycled over the clusters. Default is NULL: as set by the theme,
#'   otherwise 20.
#' @param ... Additional customization arguments for title and subtitle.
#'
#' @return Updated GC chart with new title settings.
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
#' # Basic usage
#' GC_chart(genes_data, cluster ="cluster", group = "group", height = "400px") %>%
#' GC_labels("name") %>%
#' GC_clusterTitle(
#'   title = "Cluster 1 Data",
#'   subtitle = "Detailed View",
#'   show = TRUE,
#'   cluster = 1
#' )
#'
#' # Customizing title style
#' GC_chart(genes_data, cluster = "cluster", group = "group", height = "400px") %>%
#'   GC_labels("name") %>%
#'   GC_clusterTitle(
#'     title = "Cluster 1 Data",
#'     subtitle = "Detailed View",
#'     show = TRUE,
#'     cluster = 1,
#'     x = 0,
#'     y = 5,
#'     align = "center",
#'     spacing = 20,
#'     titleFont = list(
#'       fontSize = "16px",
#'       fontStyle = "normal",
#'       fontWeight = "bold",
#'       textDecoration = "normal",
#'       fontFamily = "sans-serif",
#'       cursor = "default",
#'       fill = "black"
#'       # Any other CSS styles
#'     ),
#'     subtitleFont = list(
#'       fontSize = "14px",
#'       fontStyle = "normal",
#'       fontWeight = "bold",
#'       textDecoration = "normal",
#'       fontFamily = "sans-serif",
#'       cursor = "default",
#'       fill = "black"
#'       # Any other CSS styles
#'     )
#'   )
#' @export
GC_clusterTitle <- function(
    GC_chart,
    title = NULL,
    subtitle = NULL,
    subtitleFont = list(),
    titleFont = list(),
    show = TRUE,
    height = NULL,
    cluster = NULL,
    align = NULL,
    spacing = NULL,
    ...) {
  spec <- spec_args(match.call(), environment())
  # show = FALSE hides what an earlier call shows (B76); with nothing shown
  # before, the chart is unchanged
  if (nothing_to_hide(GC_chart, "clusterTitle", show)) {
    return(GC_chart)
  }
  check_align(align, per_cluster = TRUE)
  check_spacing(spacing, per_cluster = TRUE)
  check_clusters(GC_chart, cluster)

  add_spec(GC_chart, "clusterTitle", spec)
}

#' Update Sequence Display of a GC Chart Cluster
#'
#' Modify the sequence display and break markers of specified clusters within a GC chart.
#'
#' This function allows customization of the sequence line and break markers in a GC chart.
#' It offers options to adjust the sequence line (`sequenceStyle`) and break markers (`markerStyle`).
#' The `y` parameter can be used to set the vertical position of the sequence.
#'
#' @param GC_chart A GC chart object.
#' @param show Logical, whether to display the sequence (default is TRUE).
#' @param cluster Numeric or character vector specifying clusters to update.
#' @param y Vertical position of the sequence line. Default is NULL: as set by
#'   the theme, otherwise 50.
#' @param sequenceStyle A list of styling options for the sequence line.
#' @param markerStyle A list of styling options for the sequence break markers, e.g. `markerHeight`, `tiltAmount` and `gap` (distance in pixels between each marker and the break), plus any CSS style.
#' @param ... Additional customization arguments for sequence display.
#'
#' @return An updated GC chart with modified sequence display settings.
#'
#' @examples
#' genes_data <- data.frame(
#'   start = c(10, 90, 130, 170, 210),
#'   end = c(40, 120, 160, 200, 240),
#'   name = c('Gene 1', 'Gene 3', 'Gene 4', 'Gene 5', 'Gene 6'),
#'   group = c('A', 'B', 'B', 'A', 'C'),
#'   cluster = c(1, 1, 2, 2, 2)
#' )
#'
#' # Basic usage
#' GC_chart(genes_data, cluster ="cluster", group = "group", height = "200px") %>%
#' GC_labels("name") %>%
#' GC_sequence(show = TRUE, y = 50, cluster = NULL)
#'
#' # Customize sequence and marker styles
#' GC_chart(genes_data, cluster="cluster", group = "group", height = "200px") %>%
#'   GC_scale(hidden = TRUE, scale_breaks = TRUE) %>%
#'   GC_sequence(
#'     start = NULL,
#'     end = NULL,
#'     sequenceStyle = list(
#'       stroke = "blue",
#'       strokeWidth = 1
#'       # Any other CSS style
#'     ),
#'     markerStyle = list(
#'       stroke = "blue",
#'       strokeWidth = 1,
#'       gap = 3,
#'       tiltAmount = 5
#'       # Any other CSS style
#'     )
#'   ) %>%
#'   GC_legend(FALSE)
#'
#' @export
GC_sequence <- function(
    GC_chart,
    show = TRUE,
    cluster = NULL,
    y = NULL,
    sequenceStyle = list(),
    markerStyle = list(),
    ...
) {

  spec <- spec_args(match.call(), environment())
  check_clusters(GC_chart, cluster)

  add_spec(GC_chart, "sequence", spec, rename = c(sequenceStyle = "lineStyle"))
}

#' Update Grid Display of a GC Chart Cluster
#'
#' Modify the grid display of specified clusters within a GC chart. This
#' function allows users to adjust the margins, width, and height of the grid
#' for each cluster.
#'
#' @param GC_chart A GC chart object.
#' @param margin A list specifying top, right, bottom, and left margins.
#' @param width Numeric or character. Width of the grid. If numeric, will be
#' considered as percentage.
#' @param height Numeric. Height of the grid.
#' @param direction Character. Layout direction of the grid, either "column"
#' (default) for vertical or "row" for horizontal.
#' @param cluster Numeric or character vector. Clusters in the GC chart to update.
#'
#' @return Updated GC chart with new grid display settings.
#' @importFrom utils modifyList
#' @examples
#' genes_data <- data.frame(
#'   start = c(10, 90, 130, 170, 210),
#'   end = c(40, 120, 160, 200, 240),
#'   name = c('Gene 1', 'Gene 3', 'Gene 4', 'Gene 5', 'Gene 6'),
#'   group = c('A', 'B', 'B', 'A', 'C'),
#'   cluster = c(1, 1, 2, 2, 2)
#' )
#'
#' # Set Margin of clusters
#' GC_chart(genes_data, cluster ="cluster", group = "group", height = "200px") %>%
#' GC_grid(margin = list(left = "50px", right = "0px"))
#'
#' # Set height of a specific cluster
#' GC_chart(genes_data, cluster ="cluster", group = "group", height = "200px") %>%
#' GC_grid(height = "120px", cluster = 2)
#'
#' @export
GC_grid <- function(
    GC_chart,
    margin = NULL,
    width = NULL,
    height = NULL,
    direction = "column",
    cluster = NULL
) {

  spec <- spec_args(match.call(), environment())
  check_clusters(GC_chart, cluster)
  chart_height <- GC_chart$height

  # The width of the widget
  if(!is.null(width) && is.null(cluster)){
    GC_chart$width <- get_relative_height(GC_chart$width, width)
  }

  # Top and bottom margins are padding of the whole chart
  if(!is.null(margin) && is.null(cluster)){

    paddingTop <- if (!is.null(margin[["top"]])) get_relative_height(chart_height, margin[["top"]]) else 0
    paddingBottom <- if (!is.null(margin[["bottom"]])) get_relative_height(chart_height, margin[["bottom"]]) else 0

    if (!is.numeric(paddingTop) || !is.numeric(paddingBottom)) {
      stop("Use pixel values for the top and bottom margins when the chart height is a percentage.")
    }

    # The library takes the padding from the cluster heights; the widget
    # fills its container
    if(paddingTop > 0 || paddingBottom > 0){
      GC_chart$height <- "100%"
    }
  }

  add_spec(GC_chart, "grid", spec)
}

#' Update Scale of a GC Chart Cluster
#'
#' Modify the scale settings for specified clusters within a GC chart.
#'
#' @param GC_chart A GC chart object.
#' @param cluster Numeric or character vector specifying clusters in the GC
#' chart to update.
#' @param start Numeric vector indicating the starting point for the scale.
#' Default is NULL.
#' @param end Numeric vector indicating the end point for the scale.
#' Default is NULL.
#' @param padding Numeric value or percentage string indicating the padding on
#' either side of the scale. The value can be a number or a string in the
#' format of '2%'. Default is NULL: as set by the theme, otherwise 2.
#' @param hidden Logical flag indicating whether the axis is hidden. Default is
#' FALSE.
#' @param breaks The scale breaks: stretches of the scale that are left out,
#' with break marks on the line. Either a list of breaks for all selected
#' clusters, e.g. `list(list(start = 5486, end = 9336))`, or a data frame with
#' `start` and `end` columns and, for breaks per cluster, a `cluster` column.
#' Positions are those of the data, also with `axis_type = "range"`. Default is
#' an empty list ().
#' @param axis_position Character string indicating the type of the axis ('top' or
#' bottom'). Default is NULL: as set by the theme, otherwise 'bottom'.
#' @param axis_type Character string indicating the type of the axis ('position' or
#' 'range'). Default is 'position'.
#' @param tickValues Numeric vector or NULL, custom tick values to be used at
#' the top of the cluster. If NULL, the default tick values are used.
#' @param reverse Logical flag indicating whether to reverse the scale for the
#' corresponding cluster. Default is FALSE.
#' @param scale_breaks Logical flag indicating if scale breaks should be
#' employed. Default is FALSE.
#' @param scale_break_threshold Numeric value indicating the threshold
#' percentage for determining scale breaks. Default is 20.
#' @param scale_break_padding The room left on either side of a scale break: a
#' number, as a percentage of the region shown (without its breaks), or a
#' string in base pairs, e.g. `"500bp"`, for the same room around every break.
#' Default is 1.
#' @param ticksCount Numeric value indicating the number of ticks on the scale.
#' Default is NULL: as set by the theme, otherwise 10.
#' @param ticksFormat Format for tick labels; depends on axis_type, defaulting
#' to ",.0f" for 'position' or ".2s" for 'range' when NULL.
#' @param y Numeric value from 1 to 100 indicating the y-position of the x-axis.
#' Default is NULL.
#' @param tickStyle List specifying the style for the ticks.
#' @param textStyle List specifying the style for the tick text.
#' @param lineStyle List specifying the style for the axis line.
#' @param ... Additional arguments for scale settings.
#'
#' @return Updated GC chart with new scale settings.
#'
#' @examples
#' genes_data <- data.frame(
#'   start = c(100, 1000, 2000),
#'   end = c(150, 1500, 2500),
#'   name = c('Gene 4', 'Gene 5', 'Gene 6'),
#'   group = c('B', 'A', 'C'),
#'   cluster = c(2, 2, 2)
#' )
#' #Example usage with all custom options
#' GC_chart(genes_data, cluster = "cluster", group = "group", height = "200px") %>%
#'   GC_scale(
#'     start = 1,
#'     end = 2600,
#'     padding = 2,
#'     hidden = FALSE,
#'     reverse = FALSE,
#'     axis_position = "bottom",
#'     # breaks = list(
#'     #  list(start = 160, end = 900),
#'     #  list(start = 1600, end = 1900)
#'     # ),
#'     # tickValues = c(1, 2600),
#'     scale_breaks = TRUE,
#'     scale_break_threshold = 20,
#'     scale_break_padding = 1,
#'     ticksCount = 10,
#'     ticksFormat = ",.0f",
#'     y = NULL,
#'     tickStyle =
#'       list(
#'         stroke = "grey",
#'         strokeWidth = 1,
#'         lineLength = 6
#'         # Any other CSS styles
#'       ),
#'     textStyle =
#'       list(
#'         fill = "black",
#'         fontSize = "10px",
#'         fontFamily = "Arial",
#'         cursor = "default"
#'         # Any other CSS styles
#'       ),
#'     lineStyle = list(
#'       stroke = "grey",
#'       strokeWidth = 1
#'       # Any other CSS styles
#'     )
#'   ) %>%
#'   GC_legend(FALSE)
#' @export
GC_scale <- function(GC_chart,
                     cluster = NULL,
                     start = NULL,
                     end = NULL,
                     padding = NULL,
                     hidden = FALSE,
                     breaks = list(),
                     tickValues = NULL,
                     reverse = FALSE,
                     axis_position = NULL,
                     axis_type = "position", # range
                     y = NULL,
                     scale_breaks = FALSE,
                     scale_break_threshold = 20,
                     scale_break_padding = 1,
                     ticksCount = NULL,
                     ticksFormat = NULL,
                     tickStyle = list(),
                     textStyle = list(),
                     lineStyle = list(),
                     ...) {
  spec <- spec_args(match.call(), environment())

  # Fail early on a padding that is no percentage or base pairs
  if (is.character(scale_break_padding)) break_padding_bp(scale_break_padding)

  clusters <- getUpdatedClusters(chart_fields(GC_chart)$clusters, cluster)

  # Breaks given as a data frame, for the clusters in its cluster column
  if (!scale_breaks && length(clusters) > 0 && is.data.frame(breaks) &&
      !all(c("start", "end") %in% names(breaks))) {
    stop("A data frame of breaks needs a start and an end column")
  }

  add_spec(GC_chart, "scale", spec,
           rename = c(scale_breaks = "autoBreaks",
                      scale_break_threshold = "autoBreakThreshold",
                      scale_break_padding = "breakPadding"))
}

#' Update Scale Bar of a GC Chart Cluster
#'
#' Modify the scale bar settings for specified clusters within a GC chart.
#'
#' @param GC_chart A GC chart object.
#' @param show Logical. Whether to show the scale bar.
#' @param cluster Numeric or character vector. Clusters in the GC chart to update.
#' @param scaleBarLineStyle List of style options for the scale bar line.
#' @param scaleBarTickStyle List of style options for the scale bar ticks.
#' @param labelStyle List of style options for the scale bar label.
#' @param ... Additional arguments for scale bar settings.
#'
#' @return Updated GC chart with new scale bar settings.
#'
#' @examples
#' genes_data <- data.frame(
#'  start = c(1000, 9000, 13000, 17000, 21000),
#'  end = c(4000, 12000, 16000, 20000, 24000),
#'  name = c('Gene 1', 'Gene 3', 'Gene 4', 'Gene 5', 'Gene 6'),
#'  group = c('A', 'B', 'B', 'A', 'C'),
#'  cluster = c(1, 1, 2, 2, 2)
#' )
#'
#' # Set scale bar for individual clusters
#' GC_chart(genes_data, cluster ="cluster", group = "group") %>%
#' GC_scaleBar(cluster = 1, title = "1 kb", scaleBarUnit = 1000) %>%
#' GC_scaleBar(cluster = 2, title = "2 kb", scaleBarUnit = 2000)
#'
#' # Style scale bar
#' GC_chart(genes_data, cluster ="cluster", group = "group", height = "400px") %>%
#'   GC_scaleBar(
#'     title = "1kb",
#'     scaleBarUnit = 1000,
#'     cluster = NULL,
#'     x = 0,
#'     y = 50,
#'     labelStyle = list(
#'       labelPosition =  "left",
#'       fontSize = "10px",
#'       fontFamily = "sans-serif",
#'       fill = "red", # Text color
#'       cursor = "default"
#'       # Any other CSS style for the label
#'     ),
#'     textPadding = -2,
#'     scaleBarLineStyle = list(
#'       stroke = "black",
#'       strokeWidth = 1
#'       # Any other CSS style for the line
#'     ),
#'     scaleBarTickStyle = list(
#'       stroke = "black",
#'       strokeWidth = 1
#'       # Any other CSS style for the tick
#'     )
#'   )
#'
#' @export
GC_scaleBar <- function(
    GC_chart,
    show = TRUE,
    cluster = NULL,
    scaleBarLineStyle = list(),
    scaleBarTickStyle = list(),
    labelStyle = list(),
    ...
) {

  spec <- spec_args(match.call(), environment())
  # show = FALSE hides what an earlier call shows (B76); with nothing shown
  # before, the chart is unchanged
  if (nothing_to_hide(GC_chart, "scaleBar", show)) {
    return(GC_chart)
  }

  check_clusters(GC_chart, cluster)

  add_spec(GC_chart, "scaleBar", spec,
           rename = c(scaleBarLineStyle = "lineStyle", scaleBarTickStyle = "tickStyle"))
}

#' Set or Update Cluster Labels for a GC Chart
#'
#' This function allows you to set or update the labels for specified clusters
#' within a GC chart. It provides flexibility in terms of the title, visibility,
#' width, position, and other additional customization options.
#'
#' @param GC_chart A GC chart object.
#' @param title Character vector. The title for the cluster label. Default is NULL.
#' @param subtitle Character vector. A second line below the title, e.g. the
#' species, recycled over the clusters. Default is NULL: no subtitle.
#' @param show Logical. Whether to show the cluster label. Default is TRUE.
#' @param width Character. The width of the cluster label, which sets the left
#' or right margin of the cluster. Default is NULL: a margin set with
#' [GC_grid()], otherwise the width of the theme, otherwise "100px".
#' @param cluster Numeric or character vector. Clusters in the GC chart to update.
#' Default is NULL.
#' @param position Character. Position of the label, either "left" or "right".
#' Default is NULL: the position of the theme, otherwise "left".
#' @param align Character vector. Horizontal alignment of the lines of the
#' label in its margin, "left", "center" or "right", recycled over the
#' clusters. `x` moves the label from this position, e.g. `x = 5` for some
#' space at the left edge. Default is NULL: the alignment of the theme,
#' otherwise "center".
#' @param wrapLabel Logical. Indicates whether the label should be wrapped.
#' Default is NULL: as set by the theme, otherwise TRUE.
#' @param wrapOptions List. Specifies the wrapping options.
#' Default is an empty List.
#' @param subtitleFont List. Settings for the subtitle font, e.g.
#' `list(fontStyle = "italic")`. Default is an empty list: 11px, not bold.
#' @param spacing Numeric vector. Pixels between the middle of the last line of
#' the title and of the subtitle, recycled over the clusters. The title and
#' subtitle are centred together on the cluster. Default is NULL: as set by the
#' theme, otherwise 1.2 times the mean font size of the title and subtitle (14
#' for the default 12px and 11px).
#' @param ... Additional customization arguments for the cluster label,
#' such as 'fontSize', 'fontStyle', 'fontWeight', 'fontFamily', 'cursor', etc.
#'
#'
#' @return Updated GC chart with new or modified cluster labels.
#'
#' @examples
#' genes_data <- data.frame(
#'   start = c(10, 90, 130, 170, 210),
#'   end = c(40, 120, 160, 200, 240),
#'   name = c('Gene 1', 'Gene 3', 'Gene 4', 'Gene 5', 'Gene 6'),
#'   group = c('A', 'B', 'B', 'A', 'C'),
#'   cluster = c(1, 1, 2, 2, 2)
#' )
#'
#' # Set cluster labels
#' GC_chart(genes_data, cluster ="cluster", group = "group", height = "200px") %>%
#' GC_clusterLabel(title = unique(genes_data$cluster))
#'
#' # Set label for a specific cluster
#' GC_chart(genes_data, cluster ="cluster", group = "group", height = "200px") %>%
#' GC_clusterLabel(title = "Cluster 1", cluster = 1)
#'
#' # Add a subtitle below each label
#' GC_chart(genes_data, cluster ="cluster", group = "group", height = "200px") %>%
#'   GC_clusterLabel(
#'     title = c("pQEB1", "pQEB1_inv1"),
#'     subtitle = c("E. cloacae", "K. oxytoca"),
#'     subtitleFont = list(fontStyle = "italic"),
#'     spacing = 14
#'   )
#'
#' # Style labels
#' GC_chart(genes_data, cluster ="cluster", group = "group", height = "200px") %>%
#'   GC_clusterLabel(
#'     title = c("Cluster 1", "Cluster 2"),
#'     width = "100px",
#'     x = 0,
#'     y = 0,
#'     position = "left",
#'     align = "left", # center / right
#'     wrapLabel = TRUE,
#'     wrapOptions = list(
#'       dyAdjust = 0,
#'       lineHeightEms = 1.05,
#'       lineHeightSquishFactor =  1,
#'       splitOnHyphen =  TRUE,
#'       centreVertically = TRUE
#'      ),
#'     fontSize = "12px",
#'     fontStyle = "normal",
#'     fontWeight = "bold",
#'     fontFamily = "sans-serif",
#'     cursor = "default"
#'   )
#'
#' @export
GC_clusterLabel <- function(
    GC_chart,
    title = NULL,
    show = TRUE,
    width = NULL,
    cluster = NULL,
    position = NULL,
    align = NULL,
    wrapLabel = NULL,
    wrapOptions = list(),
    subtitle = NULL,
    subtitleFont = list(),
    spacing = NULL,
    ...
) {

  spec <- spec_args(match.call(), environment())
  # show = FALSE hides what an earlier call shows (B76); with nothing shown
  # before, the chart is unchanged
  if (nothing_to_hide(GC_chart, "clusterLabel", show)) {
    return(GC_chart)
  }
  check_align(align, per_cluster = TRUE)
  check_spacing(spacing, per_cluster = TRUE)
  check_clusters(GC_chart, cluster)

  add_spec(GC_chart, "clusterLabel", spec)
}

#' Add a Footer to Each Cluster in a GC Chart
#'
#' This function allows you to add a footer to all or specific clusters within a GC chart.
#' You can specify titles, subtitles, and control the display and styling.
#'
#' @param GC_chart A GC chart object that the footers will be added to.
#' @param title Character vector or NULL. The title to be displayed in the footer.
#'        Multiple titles can be provided for different clusters, and they will be recycled
#'        if there are more clusters than titles. Default is NULL.
#' @param subtitle Character vector or NULL. Subtitles to accompany the main titles.
#'        Default is NULL.
#' @param show Logical vector. Controls the visibility of each footer.
#'        Default is TRUE for all clusters.
#' @param cluster Numeric or character vector specifying which clusters should have footers added or updated.
#'        If NULL, all clusters will be updated. Default is NULL.
#' @param subtitleFont List, styling options for the subtitle.
#' @param titleFont List, styling options for the title.
#' @param align Character vector. Horizontal alignment of the title and
#'   subtitle, "left", "center" or "right", recycled over the clusters. `x`
#'   moves them from this position. Default is NULL: the alignment of the
#'   theme, otherwise "left".
#' @param spacing Numeric vector. Pixels between the title and the subtitle,
#'   recycled over the clusters. Default is NULL: as set by the theme,
#'   otherwise 12.
#' @param ... Additional arguments for further customization of the footers.
#'
#' @return A GC chart object with updated footer settings for each specified cluster.
#'
#' @examples
#' genes_data <- data.frame(
#'   start = c(10, 90, 130, 170, 210),
#'   end = c(40, 120, 160, 200, 240),
#'   name = c('Gene 1', 'Gene 3', 'Gene 4', 'Gene 5', 'Gene 6'),
#'   group = c('A', 'B', 'B', 'A', 'C'),
#'   cluster = c(1, 1, 2, 2, 2)
#' )
#'
#' # Add a simple footer with subtitle to all clusters
#' GC_chart(genes_data, cluster = "cluster", group = "group") %>%
#'   GC_clusterFooter(
#'     title = "Cluster Footer",
#'     subtitle = "Cluster subtitle"
#'   )
#' # Add styling to the title and subtitle
#' GC_chart(genes_data, cluster = "cluster", group = "group") %>%
#'   GC_clusterFooter(
#'     title = "This is a footer",
#'     subtitle = "Subtitle for the footer",
#'     show = TRUE,
#'     cluster = 1,
#'     x = 6,
#'     y = -20,
#'     align = "center", # left / right
#'     spacing = 12,
#'     titleFont = list(
#'       fontSize = "12px",
#'       fontWeight = "bold",
#'       fontFamily = "sans-serif",
#'       fill = "black",
#'       cursor = "default"
#'     ),
#'     subtitleFont = list(
#'       fill = "grey",
#'       fontSize = "10px",
#'       fontStyle = "normal",
#'       fontFamily = "sans-serif",
#'       cursor = "default"
#'     )
#'   )
#'
#' @export
GC_clusterFooter <- function(
    GC_chart,
    title = NULL,
    subtitle = NULL,
    show = TRUE,
    cluster = NULL,
    subtitleFont = list(),
    titleFont = list(),
    align = NULL,
    spacing = NULL,
    ...
) {

  spec <- spec_args(match.call(), environment())
  # show = FALSE hides what an earlier call shows (B76); with nothing shown
  # before, the chart is unchanged
  if (nothing_to_hide(GC_chart, "clusterFooter", show)) {
    return(GC_chart)
  }
  check_align(align, per_cluster = TRUE)
  check_spacing(spacing, per_cluster = TRUE)
  check_clusters(GC_chart, cluster)

  add_spec(GC_chart, "clusterFooter", spec)
}


#' Modify Coordinates in a GC Chart
#'
#' This function updates a GC chart by modifying the coordinates settings. It
#' allows for showing or hiding tick values, applying custom tick values for the
#' top and bottom axes, and supports several other customizations for specific
#' or all clusters in the chart.
#'
#' @param GC_chart The GC chart object to be modified.
#' @param show Logical, whether to show the tick values or not. Can be a single
#'   value or a vector.
#' @param tickValuesTop Numeric vector or NULL, custom tick values to be used at
#'   the top of the cluster. If NULL, the default tick values are used.
#' @param tickValuesBottom Numeric vector or NULL, custom tick values to be used
#'   at the bottom of the cluster. If NULL, the default tick values are used.
#' @param ticksFormat Format for tick labels. Default is ",.0f".
#' @param cluster Numeric or character vector or NULL; specifies which clusters
#' to generate coordinates for.
#'        If NULL, labels will be applied to all clusters. Default is NULL.
#' @param tickStyle List, styling options for the ticks.
#' @param textStyle List, styling options for the text.
#' @param groupStyle Named list of styles by group, applied to the coordinates
#'   of all genes of that group (the `group` of [GC_chart()], or the column
#'   given in `style_by`). `show = FALSE` leaves out their coordinates, styles
#'   in `tick` go to their ticks and the other styles to their text, e.g.
#'   `list(other = list(show = FALSE), tnsA = list(fill = "red"))`. Only for
#'   the coordinates computed from the genes, not for `tickValuesTop` or
#'   `tickValuesBottom`.
#' @param style_by Column name or NULL; the column whose values `groupStyle` is
#'   matched against. Default is NULL: the `group` of [GC_chart()].
#'   the coordinate modifications. If NULL, applies to all clusters.
#' @param ... Additional arguments to be passed to the coordinate options.
#'
#' @return Returns the GC chart object with updated coordinates.
#'
#' @examples
#' genes_data <- data.frame(
#'   start = c(10, 90, 130, 170, 210),
#'   end = c(40, 120, 160, 200, 240),
#'   name = c('Gene 1', 'Gene 2', 'Gene 3', 'Gene 4', 'Gene 5'),
#'   cluster = c(1, 1, 2, 2, 2)
#' )
#'
#' # Add coordinates to all clusters
#' GC_chart(genes_data, cluster = "cluster", group = "name", height = "200px") %>%
#' GC_coordinates()
#'
#' # Modify coordinates of a specific cluster
#' GC_chart(genes_data, cluster = "cluster", group = "name", height = "200px") %>%
#' GC_coordinates() %>%
#' GC_coordinates(
#'   cluster = 2,
#'   show = TRUE,
#'   tickValuesTop = c(130, 170, 210, 240),
#'   tickValuesBottom = c(160, 200),
#'   ticksFormat = ",.0f", # ".2s",
#'   rotate = -45,
#'   yPositionTop = 55,
#'   yPositionBottom = 45,
#'   overlapThreshold = 20,
#'   tickStyle = list(
#'   stroke = "black",
#'   strokeWidth = 1,
#'   lineLength = 6
#'   ),
#'   textStyle = list(
#'   fill = "black",
#'   fontSize = "12px",
#'   fontFamily = "Arial",
#'   cursor = "default"
#'   )
#' )
#'
#' @export
GC_coordinates <- function(
    GC_chart,
    show = TRUE,
    tickValuesTop = NULL,
    tickValuesBottom = NULL,
    ticksFormat = NULL,
    tickStyle = list(),
    textStyle = list(),
    cluster = NULL,
    groupStyle = NULL,
    style_by = NULL,
    ...
) {

  spec <- spec_args(match.call(), environment())
  state <- chart_fields(GC_chart)
  check_group_style(groupStyle)
  check_style_by(state, style_by)
  # the column groupStyle is matched against
  if (is.null(style_by)) style_by <- state$group
  if (!is.null(groupStyle) && is.null(style_by)) {
    stop("groupStyle needs a group in GC_chart() or a style_by column")
  }

  # show = FALSE hides what an earlier call shows (B76); with nothing shown
  # before, the chart is unchanged
  if (nothing_to_hide(GC_chart, "coordinates", show)) {
    return(GC_chart)
  }

  getUpdatedClusters(state$clusters, cluster)

  add_spec(GC_chart, "coordinates", spec)
}


#' Modify Cluster Settings
#'
#' This function can switch prevention of gene overlap on, adjust the spacing
#' between tracks and alter the styling of specified clusters.
#'
#' @param GC_chart The gene chart object to be modified.
#' @param separate_strands Logical, indicating whether to vertically separate
#' forward and reverse genes.
#' @param strand_spacing Numeric, specifies the spacing between genes on
#' different strands. Used only if `separate_strands` is TRUE.
#' @param prevent_gene_overlap Whether to put overlapping genes on separate
#' tracks, below each other: `TRUE` for all genes that overlap, or `"nested"`
#' for only the genes that lie inside another gene (e.g. an ORF inside another
#' reading frame), which go on the tracks below; the other genes stay on the
#' first track, also where they overlap. A gene goes on the first track where
#' it overlaps no gene placed before it. The track of each gene is in the
#' `geneTrack` column, for the `style_by` of [GC_genes()] and [GC_labels()],
#' e.g. to draw the genes of the second track smaller.
#' @param overlap_min The overlap two genes need to go on separate tracks with
#' `prevent_gene_overlap`: a number of base pairs, or a share of the shorter
#' gene as a string, e.g. `"10%"`. Genes that overlap by no more, e.g. two
#' genes that share a base or a stop codon, stay on the same track. Default
#' NULL: any overlap.
#' @param overlap_spacing Numeric, specifies the spacing between overlapping genes
#' Used only if `prevent_gene_overlap` is TRUE.
#' @param cluster Optional; used to specify which clusters in the chart should have tooltips.
#' @param style A list of CSS styles to be applied to the gene track.
#'              Each element of the list should be a valid CSS property-value
#'              pair. For example, list(backgroundColor = "red", color = "white").
#' @param ... Additional arguments to be passed to the underlying functions.
#'
#' @return Returns the modified gene chart object.
#'
#' @examples
#' genes_data <- data.frame(
#'   start = c(1, 10, 200, 220, 600),
#'   end = c(10, 150, 180, 400, 400),
#'   name = c('Gene 1', 'Gene 2', 'Gene 3', 'Gene 4', 'Gene 5'),
#'   group = c('A', 'A', 'A', 'A', 'A')
#' )
#'
#'
#' GC_chart(genes_data, group = "group", height = "150px") %>%
#'   GC_cluster(separate_strands=TRUE, strand_spacing = 0) %>%
#'   GC_legend(FALSE)
#'
#' # Only the gene inside another one goes below, drawn smaller
#' nested_genes <- data.frame(
#'   start = c(1, 120, 300, 340), end = c(125, 400, 380, 600),
#'   name = c("A", "B", "B2", "C")
#' )
#' GC_chart(nested_genes, height = "150px") %>%
#'   GC_cluster(prevent_gene_overlap = "nested") %>%
#'   GC_genes(style_by = "geneTrack",
#'            groupStyle = list("2" = list(markerHeight = 8, arrowheadHeight = 8))) %>%
#'   GC_labels("name")
#'
#' @export
GC_cluster <- function(
    GC_chart,
    separate_strands = NULL,
    strand_spacing = NULL,
    prevent_gene_overlap = NULL,
    overlap_min = NULL,
    overlap_spacing=NULL,
    cluster = NULL,
    style = list(),
    ...
) {

  spec <- spec_args(match.call(), environment())
  if (!is.null(prevent_gene_overlap) &&
      !(length(prevent_gene_overlap) == 1 && !is.na(prevent_gene_overlap) &&
        (is.logical(prevent_gene_overlap) || identical(prevent_gene_overlap, "nested")))) {
    stop("'prevent_gene_overlap' must be TRUE, FALSE or \"nested\".")
  }
  if (!is.null(overlap_min) &&
      !(length(overlap_min) == 1 && !is.na(overlap_min) &&
        ((is.numeric(overlap_min) && overlap_min >= 0) ||
         (is.character(overlap_min) && grepl("^[0-9]+(\\.[0-9]+)?%$", overlap_min) &&
          as.numeric(sub("%$", "", overlap_min)) <= 100)))) {
    stop("'overlap_min' must be a number of base pairs or a share such as \"10%\".")
  }

  if (isTRUE(separate_strands) && !is.null(prevent_gene_overlap) &&
      !isFALSE(prevent_gene_overlap)) {
    warning("Setting both 'separate_strands' and 'prevent_gene_overlap' to TRUE is not supported. Resetting to NULL.")
  }
  check_clusters(GC_chart, cluster)

  add_spec(GC_chart, "cluster", spec)
}
