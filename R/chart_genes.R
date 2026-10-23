#' Add Labels to Each Cluster in a GC Chart
#'
#' This function adds labels to each cluster within a GC chart. It provides the
#' option to show or hide the labels and supports customization of label properties.
#' It can automatically pick up group names as labels from the `GC_chart` object if not provided.
#'
#' @param GC_chart A `GC chart` object to which labels will be added.
#' @param label Character vector, logical, or NULL. Specific labels for the clusters.
#'        If NULL and `GC_chart` has group names, those will be used as labels.
#' @param show Logical; controls the visibility of labels. Default is `TRUE`.
#' @param cluster Numeric or character vector or NULL; specifies which clusters should be labeled.
#'        If NULL, labels will be applied to all clusters. Default is NULL.
#' @param itemStyle List, a list of styles to apply to individual items in the chart.
#' @param ... Additional arguments for further customization of the labels.
#'
#' @return A `GC chart` object with updated label settings for each specified cluster.
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
#' # Add labels to all clusters
#' GC_chart(genes_data, cluster = "cluster", group = "group", height = "200px") %>%
#' GC_labels()
#'
#' # Add labels and styling
#' GC_chart(genes_data, cluster = "cluster", group = "group", height = "200px") %>%
#' GC_labels(
#'  label = "group",
#'  show = TRUE,
#'  x = 0,
#'  y = 50,
#'  dy = "-1.2em",
#'  dx = "0em",
#'  rotate = 0,
#'  adjustLabels = TRUE, # Rotate labels to prevent overlap
#'  fontSize = "12px",
#'  fontStyle = "italic",
#'  fill = "black",
#'  fontFamily = "sans-serif",
#'  textAnchor = "middle",
#'  cursor = "default",
#' )
#'
#' # Alter style of a specific label
#' GC_chart(genes_data, cluster = "cluster", group = "group", height = "200px") %>%
#' GC_labels(label = "group") %>%
#' GC_labels(
#'   cluster = 1,
#'   itemStyle = list(
#'     list(index = 0, fill = "red", fontSize = "14px", fontWeight = "bold")
#'   )
#' )
#'
#' @export
GC_labels <- function(
    GC_chart,
    label = NULL,
    show = TRUE,
    cluster = NULL,
    itemStyle = list(),
    ...
) {

  if (!any(show)) {
    return(GC_chart)
  }

  if (is.logical(label) && length(label) == 1) {
    show <- label
    label <- NULL
  }

  if (is.null(label) && is.null(GC_chart$x$group)){
    stop("Please define labels")
  }

  if (is.null(label) && !is.null(GC_chart$x$group)) {
    label <- GC_chart$x$group
  }

  if (!all(label %in% names(GC_chart$x$data))) {
    stop("label column not found in data")
  }

  # Update the GC_chart object with title and options for each cluster
  clusters <- getUpdatedClusters(GC_chart, cluster)

  for(i in seq_along(clusters)){

    # Default options
    options <- Filter(function(x) !is.null(x) && length(x) > 0, list(
      label = recycle_at(label, i),
      show = recycle_at(show, i),
      itemStyle = itemStyle,
      ...
    ))

    # Merge new options with existing options
    if (is.null(GC_chart$x$series[[clusters[i]]]$labels)) {
      GC_chart$x$series[[clusters[i]]]$labels <- options
    } else {
      GC_chart$x$series[[clusters[i]]]$labels <- modifyList(GC_chart$x$series[[clusters[i]]]$labels, options)
    }
  }

  return(GC_chart)
}

#' Modify Gene Characteristics within a Chart
#'
#' This function updates a gene chart with specific characteristics for genes
#' based on the given parameters. It can show/hide genes, apply a color scheme,
#' assign custom colors, filter by cluster, and accept additional options.
#'
#' @param GC_chart The gene chart object to be modified.
#' @param group Column name used for gene grouping to influence color
#'   aesthetics.
#' @param marker Character or NULL, type of marker to represent genes on the chart.
#' Allowed values are 'arrow', 'boxarrow', 'box', 'cbox', and 'rbox'.
#' @param corner_radius Numeric or NULL, radius in pixels of the rounded corners
#'   of 'arrow', 'boxarrow', 'box' and 'rbox' markers, e.g. `3` for soft
#'   corners. The inner corners of arrows stay sharp. Default is NULL: sharp
#'   corners, and 5 for 'rbox'.
#' @param marker_size Character or NULL, size category of the marker
#' ('small', 'medium', 'large').
#' @param show Logical, whether to show the genes or not.
#' @param colorScheme Character or NULL, the name of the color scheme to use: a
#'   d3 scheme (e.g. "schemeSet2") or a geneviewer palette (e.g. "okabeIto",
#'   see [geneviewer_palette()]). When no `cluster` is selected, the legend uses the same colours. NULL keeps
#'   the colours set by `GC_color()`.
#' @param customColors List or NULL, custom colors to apply to the genes: a
#'   vector of colours, recycled over the groups, or a named list of colours by
#'   group. Groups left out of a named list keep the colours of the colour
#'   scheme. When no `cluster` is selected, the legend uses the same colours.
#' @param cluster Numeric or character, the specific cluster to filter genes by.
#' @param itemStyle List, a list of styles to apply to individual items in the
#'   chart.
#' @param ... Additional arguments to be passed to the gene options.
#'
#' @return Returns the modified gene chart object.
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
#' # Change the appearance of a specific gene cluster
#' GC_chart(genes_data, cluster = "cluster", group = "group", height = "200px") %>%
#'   GC_genes(
#'     group = "group",
#'     show = TRUE,
#'     marker = "arrow",
#'     marker_size = "medium",
#'     colorScheme = NULL, # One of D3.js build in colorSchemes
#'                         # (eg. "schemeCategory10",
#'                         # "schemeAccent", "schemeTableau10")
#'     customColors = NULL, # A vector of color names
#'     prevent_overlap = FALSE,
#'     gene_overlap_spacing = 40,
#'     cluster = 1, # Specify a specific cluster
#'     x = 1,
#'     y = 50,
#'     stroke = "black",
#'     strokeWidth = 1,
#'     arrowheadWidth = NULL,
#'     arrowheadHeight = NULL,
#'     arrowHeight = NULL,
#'     markerHeight = NULL # overwrites marker_size
#'    )
#'
#' # Change the appearance of a specific gene
#' GC_chart(genes_data, cluster = "cluster", group = "group", height = "200px") %>%
#' GC_genes(
#'   cluster = 2,
#'   itemStyle = list(list(index = 2, fill = "red", stroke = "blue")
#'   )
#'  )
#'
#' @export
GC_genes <- function(
    GC_chart,
    group = NULL,
    marker = NULL,
    marker_size = NULL,
    show = TRUE,
    colorScheme = NULL,
    customColors = NULL,
    cluster = NULL,
    itemStyle = list(),
    corner_radius = NULL,
    ...
) {

  if (is.logical(group) && length(group) == 1) {
    show <- group
    group <- NULL
  }

  # if (is.null(group) && is.null(GC_chart$x$group) && is.null(marker)){
  #   stop("Please define a group")
  # }

  if (is.null(group) && !is.null(GC_chart$x$group)){
    group <- GC_chart$x$group
  }

  if (!is.null(group) && !all(group %in% names(GC_chart$x$data))) {
    stop("group column not found in data")
  }

  # Update the GC_chart object with title and options for each cluster
  clusters <- getUpdatedClusters(GC_chart, cluster)

  for(i in seq_along(clusters)){

    # Default options
    options <- Filter(function(x) !is.null(x) && length(x) > 0, list(
      group = recycle_at(group, i),
      marker = recycle_at(marker, i),
      cornerRadius = corner_radius,
      markerSize = marker_size,
      show = recycle_at(show, i),
      colorScheme = colorScheme,
      customColors = customColors,
      itemStyle = itemStyle,
      ...
    ))

    GC_chart$x$series[[clusters[i]]]$genes <-
      keep_color_options(options, GC_chart$x$series[[clusters[i]]]$genes)

  }

  # The legend shows all clusters, so it only follows colours set for all of them
  if (is.null(cluster)) {
    GC_chart <- set_legend_colors(GC_chart, colorScheme, customColors)
  }

  # Check if prevent_gene_overlap is called last
  if (!is.null(GC_chart$x$prevent_gene_overlap_called) && GC_chart$x$prevent_gene_overlap_called) {
    warning("Preventing gene overlap must be called after setting genes, labels or coordinates for proper effect.")
  }

  return(GC_chart)
}

#'Modify Transcript Characteristics within a Chart
#'
#'This function updates the chart with specific characteristics for transcripts
#'based on the given parameters. It can show/hide transcripts, apply a color
#'scheme, assign custom colors, filter by cluster, and accept additional
#'options.
#'
#'@param GC_chart The chart object to be modified.
#'@param transcript Optional column name used for clustering transcript data.
#' Default is NULL.
#'@param type Column name identifying the feature type ("UTR",
#' "exon"). Default is NULL.
#' @param strand Optional column name indicating strand orientation. Acceptable
#'   values include 1, 'forward', 'sense', or '+' to represent the forward
#'   strand, and -1, 0, 'reverse', 'antisense', "complement" or '-' to represent
#'   the reverse strand. Default is NULL, meaning strand information is not
#'   used.
#'@param group Optional column name used for transcript grouping to influence
#'color aesthetics.
#'@param selection Numeric or character, the specific transcript to filter
#'  transcripts by.
#'@param show Logical, whether to show the transcripts or not.
#'@param colorScheme Character or NULL, the name of the color scheme to use: a
#'  d3 scheme (e.g. "schemeSet2") or a geneviewer palette (e.g. "okabeIto",
#'  see [geneviewer_palette()]). When no `selection` is made, the legend uses the same colours. NULL keeps
#'  the colours set by `GC_color()`.
#'@param customColors List or NULL, custom colors to apply to the genes: a
#'  vector of colours, recycled over the groups, or a named list of colours by
#'  group. Groups left out of a named list keep the colours of the colour
#'  scheme. When no `selection` is made, the legend uses the same colours.

#'@param styleExons List, styles to apply to exons in the chart.
#'@param styleIntrons List, styles to apply to introns in the chart.
#'@param styleUTRs List, styles to apply to UTRs in the chart.
#'@param itemStyleExons List, a list of styles to apply to individual exons in
#'  the chart.
#'@param itemStyleIntrons List, a list of styles to apply to individual introns
#'  in the chart.
#'@param itemStyleUTRs List, a list of styles to apply to individual UTRs in the
#'  chart.
#' @param labelOptions List, options for styling labels such as font size, color,
#'  and position.
#'@param ... Additional arguments to be passed to the gene options.
#'
#'@details The first call of `GC_transcript()` makes the chart again with the
#'  transcripts as clusters. Settings of the whole chart made before it (e.g.
#'  [GC_title()], [GC_legend()], [GC_theme()] and the margins of [GC_grid()])
#'  are kept. Settings per cluster (e.g. [GC_labels()], [GC_clusterTitle()])
#'  are for the old clusters: set them after `GC_transcript()`.
#'
#'@return Returns the modified gene chart object.
#'
#' @examples
#' transcript_data <- data.frame(
#'   transcript = c("transcript1", "transcript1", "transcript1", "transcript1",
#'                  "transcript2", "transcript2", "transcript2"),
#'   type = c("5_utr", "exon", "exon", "3_utr",
#'            "5_utr", "exon", "3_utr"),
#'   start = c(1, 101, 201, 301,
#'             1, 101, 301),
#'   end = c(50, 150, 250, 350,
#'           50, 150, 350),
#'   strand = rep("forward", 7)
#' )
#'
#' # All default transcript settings
#' GC_chart(
#'   transcript_data,
#'   start = "start",
#'   end = "end",
#'   height = "200px"
#' ) %>%
#'   GC_transcript(
#'     transcript = "transcript",
#'     strand = "strand",
#'     type = "type",
#'     group = NULL,
#'     show = TRUE,
#'     selection = NULL,
#'     colorScheme = NULL,
#'     customColors = NULL,
#'     styleExons = list(
#'       show = TRUE,
#'       strokeWidth = 0,
#'       cursor = "default",
#'       marker = "box",
#'       markerSize = "medium",
#'       arrowheadWidth = NULL,
#'       arrowheadHeight = NULL,
#'       markerHeight = NULL,
#'       cornerRadius = NULL
#'       # Any other CSS style
#'     ),
#'     styleIntrons = list(
#'       show = TRUE,
#'       strokeWidth = 1,
#'       fill = "none",
#'       cursor = "default",
#'       marker = "intron",
#'       markerSize = "medium",
#'       arrowheadWidth = NULL,
#'       arrowheadHeight = NULL,
#'       markerHeight = NULL,
#'       cornerRadius = NULL
#'       # Any other CSS style
#'     ),
#'     styleUTRs = list(
#'       show = TRUE,
#'       fontSize = "10px",
#'       fontStyle = "normal",
#'       fontFamily = "sans-serif",
#'       cursor = "default",
#'       color = "black",
#'       fill = "#FFF",
#'       strokeWidth = 1,
#'       marker = "box",
#'       markerSize = "medium",
#'       arrowheadWidth = NULL,
#'       arrowheadHeight = NULL,
#'       markerHeight = NULL,
#'       cornerRadius = NULL
#'       # Any other CSS style
#'     ),
#'     labelOptions = list(
#'       show = TRUE,
#'       xOffset = 2,
#'       yOffset = 0,
#'       fontSize = "12px",
#'       fontStyle = "normal",
#'       fontWeight = "normal",
#'       fontFamily = "sans-serif",
#'       cursor = "default",
#'       color = "black"
#'     ),
#'     itemStyleExons = list(),
#'     itemStyleIntrons = list(),
#'     itemStyleUTRs = list()
#'   )
#' # Change the appearance of a specific intron
#' GC_chart(transcript_data,
#'          start = "start",
#'          end = "end",
#'          height = "200px"
#' ) %>%
#'   GC_transcript(
#'     transcript = "transcript",
#'     type = "type",
#'     selection = 2,
#'     itemStyleExons = list(list(index = 0, fill = "red")
#'     )
#'   )
#' @export
GC_transcript <- function(GC_chart,
                          transcript = NULL,
                          type = NULL,
                          strand = NULL,
                          group = NULL,
                          selection = NULL,
                          show = TRUE,
                          colorScheme = NULL,
                          customColors = NULL,
                          styleExons = list(),
                          styleIntrons = list(),
                          styleUTRs = list(),
                          itemStyleExons = list(),
                          itemStyleIntrons = list(),
                          itemStyleUTRs = list(),
                          labelOptions = list(),
                          ...) {

  transcript_called <- GC_chart$x$transcript_called

  # Update GC_chart object with new data
  if (is.null(transcript_called)) {

    data <- GC_chart$x$data
    data_tmp <- GC_chart$x$data
    colnames_data <- colnames(data)

    if (!is.null(type) && length(type) != 1) {
      stop("type must be a single column name")
    }

    if (!is.null(type) && !(type %in% colnames_data)) {
      stop("type column not found in data")
    }

    if (!is.null(type)) {
      data$type <- data_tmp[[type]]
    }

    if (!is.null(transcript) && length(transcript) != 1) {
      stop("transcript must be a single column name")
    }

    if (!is.null(transcript) && !(transcript %in% colnames_data)) {
      stop("transcript column not found in data")
    }

    if (!is.null(transcript)) {
      data$transcript <- as.character(data_tmp[[transcript]])
    }
    data <- get_introns(data)
    transcripts <- unique(as.character(data$transcript))

    # Update chart object
    new_params <- GC_chart$x$params
    new_params$data <- data

    if (is.null(transcript)) {
      new_params$cluster <- "transcript"
    } else {
      new_params$cluster <- transcript
    }
    new_params$strand <- strand

    # The chart is made again with the transcripts as clusters; keep the
    # settings of the whole chart
    old_chart <- GC_chart
    GC_chart <- do.call(geneviewer::GC_chart, new_params)
    GC_chart <- keep_chart_settings(GC_chart, old_chart)

    for (trans in transcripts) {
        GC_chart$x$series[[trans]]$genes <- list(group = NULL, show = FALSE)
        GC_chart$x$series[[trans]]$sequence <- list(show = FALSE)
        GC_chart$x$series[[trans]]$transcript <- list(group = NULL, show = TRUE)
    }
  }

  if (is.logical(group) && length(group) == 1) {
    show <- group
    group <- NULL
  }

  if (is.null(group) && !is.null(GC_chart$x$group)) {
    group <- GC_chart$x$group
  }

  if (!is.null(group) && !all(group %in% names(GC_chart$x$data))) {
    stop("group column not found in data")
  }

  clusters <- getUpdatedClusters(GC_chart, selection)

  for (i in seq_along(clusters)) {
    # Default options
    options <- Filter(
      function(x)
        ! is.null(x) && length(x) > 0,
      list(
        group = recycle_at(group, i),
        show = recycle_at(show, i),
        colorScheme = colorScheme,
        customColors = customColors,
        styleExons = styleExons,
        styleIntrons = styleIntrons,
        styleUTRs = styleUTRs,
        itemStyleExons = itemStyleExons,
        itemStyleIntrons = itemStyleIntrons,
        itemStyleUTRs = itemStyleUTRs,
        labelOptions = labelOptions,
        ...
      )
    )

    GC_chart$x$series[[clusters[i]]]$transcript <-
      keep_color_options(options, GC_chart$x$series[[clusters[i]]]$transcript)

  }

  # The legend shows all transcripts, so it only follows colours set for all of them
  if (is.null(selection)) {
    GC_chart <- set_legend_colors(GC_chart, colorScheme, customColors)
  }

  GC_chart$x$transcript_called <- TRUE

  return(GC_chart)
}

#' Update Color Scheme in Gene Chart
#'
#' This function updates the color scheme of the legend and genes in a gene chart.
#'
#' @param GC_chart The gene chart object to be modified.
#' @param colorScheme Optional; character or NULL, the name of a predefined
#' color scheme to apply to the genes. Acceptable values include D3.js's built-in
#' color schemes like "schemeCategory10", "schemeAccent", "schemeTableau10",
#' and the geneviewer palettes like "okabeIto" (see [geneviewer_palette()]).
#' @param customColors Either NULL, a list of color values, or a named list of
#' color values by group. Groups left out of a named list keep the colours of
#' `colorScheme`, the palette of the theme or the default colours, e.g.
#' `customColors = list(Other = "grey")` only changes the colour of "Other".
#'
#' @return Returns the gene chart object with updated color settings for the genes.
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
#' GC_chart(genes_data, cluster = "cluster", group = "group", height = "100px") %>%
#'   GC_color(colorScheme = "schemeCategory10")
#'
#' GC_chart(genes_data, cluster = "cluster", group = "group", height = "100px") %>%
#'   GC_color(customColors = c("red", "orange", "green"))
#'
#' GC_chart(genes_data, cluster = "cluster", group = "group", height = "100px") %>%
#'   GC_color(customColors = list(A = "yellow", B = "pink", C =  "purple"))
#'
#' # Set the colour of one group; the others keep the colours of the scheme
#' GC_chart(genes_data, cluster = "cluster", group = "group", height = "100px") %>%
#'   GC_color(colorScheme = "schemeSet2", customColors = list(C = "grey"))
#'
#'
#' @export
GC_color <- function(
    GC_chart,
    colorScheme = NULL,
    customColors = NULL
) {

  # colorScheme only colours the groups left out of a named list of customColors
  if (!is.null(colorScheme) && !is.null(customColors) && is.null(names(customColors))) {
    warning("Both colorScheme and customColors are selected. Use a named list of customColors to set the colours of some groups, or choose only one.")
    return(GC_chart)
  }

  # Update legend
  GC_chart$x$legend$legendOptions$colorScheme <- colorScheme
  GC_chart$x$legend$legendOptions$customColors <- customColors

  # Capture clusters to update
  clusters <- names(GC_chart$x$series)

  # Update gene and transcript colors
  for(cluster_name in clusters){
    GC_chart$x$series[[cluster_name]]$genes$colorScheme <- colorScheme
    GC_chart$x$series[[cluster_name]]$genes$customColors <- customColors
    GC_chart$x$series[[cluster_name]]$transcript$colorScheme <- colorScheme
    GC_chart$x$series[[cluster_name]]$transcript$customColors <- customColors
  }

  return(GC_chart)
}

#' Fill Genes with Patterns
#'
#' Fills the genes, exons and legend markers with a pattern per group, for
#' example for black and white figures. Patterns can also be drawn on top of the
#' gene colours to show each group with both a colour and a pattern.
#'
#' @param GC_chart The gene chart object to be modified.
#' @param patterns NULL, a vector of pattern names or a named list of pattern
#'   names by group. NULL gives each group a pattern in a fixed order. A vector
#'   is recycled over the groups. Available patterns: "none", "solid",
#'   "horizontal", "vertical", "diagonal", "antidiagonal", "grid",
#'   "crosshatch", "dots", "zigzag", "checkerboard", "rings", "bricks",
#'   "plus", "wideStripes" and "inverseDots" (background-coloured dots on a
#'   solid tile).
#' @param color The colour of the pattern lines and dots: a single colour, a
#'   vector of colours recycled over the groups, or a named list of colours by
#'   group. For example, `color = "white"` with `background = "black"` gives
#'   white patterns on black.
#' @param background The colour behind the pattern: a single colour, a vector
#'   of colours recycled over the groups, or a named list of colours by group.
#'   NULL keeps the gene colours set by `GC_color()`. When `background` is not
#'   NULL, intron lines and UTR outlines take `color`.
#' @param size Numeric, the width and height of one pattern tile in pixels.
#' @param strokeWidth Numeric, the width of the pattern lines in pixels.
#'
#' @return Returns the gene chart object with updated pattern settings.
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
#' # Black and white
#' GC_chart(genes_data, cluster = "cluster", group = "group", height = "150px") %>%
#'   GC_pattern()
#'
#' # Chosen patterns on grey backgrounds
#' GC_chart(genes_data, cluster = "cluster", group = "group", height = "150px") %>%
#'   GC_pattern(
#'     patterns = list(A = "dots", B = "crosshatch", C = "none"),
#'     background = c("white", "grey80")
#'   )
#'
#' # Patterns on top of the gene colours
#' GC_chart(genes_data, cluster = "cluster", group = "group", height = "150px") %>%
#'   GC_pattern(background = NULL)
#'
#' # White patterns on black for group C
#' GC_chart(genes_data, cluster = "cluster", group = "group", height = "150px") %>%
#'   GC_pattern(
#'     patterns = list(A = "checkerboard", B = "rings", C = "bricks"),
#'     color = list(A = "black", B = "black", C = "white"),
#'     background = list(A = "white", B = "white", C = "black")
#'   )
#'
#' @export
GC_pattern <- function(
    GC_chart,
    patterns = NULL,
    color = "black",
    background = "white",
    size = 6,
    strokeWidth = 1
) {

  pattern <- pattern_to_js(list(
    patterns = patterns,
    color = color,
    background = background,
    size = size,
    strokeWidth = strokeWidth
  ))

  GC_chart$x$legend$legendOptions$pattern <- pattern

  for(cluster_name in names(GC_chart$x$series)){
    GC_chart$x$series[[cluster_name]]$genes$pattern <- pattern
    GC_chart$x$series[[cluster_name]]$transcript$pattern <- pattern
  }

  return(GC_chart)
}

# Pattern settings of GC_pattern() and GC_theme(pattern = ) as sent to the
# browser; NULL settings are dropped
pattern_to_js <- function(pattern) {
  available <- c("none", "solid", "horizontal", "vertical", "diagonal",
                 "antidiagonal", "grid", "crosshatch", "dots", "zigzag",
                 "checkerboard", "rings", "bricks", "plus", "wideStripes",
                 "inverseDots")
  unknown <- setdiff(unlist(pattern$patterns), available)
  if (length(unknown) > 0) {
    stop("Unknown pattern(s): ", paste(unknown, collapse = ", "),
         ". Available patterns: ", paste(available, collapse = ", "))
  }

  # R colour names such as "grey75" are not CSS colours; pass them as hex
  as_css_color <- function(x) {
    vapply(x, function(col) {
      tryCatch({
        rgba <- grDevices::col2rgb(col, alpha = TRUE)
        grDevices::rgb(rgba[1], rgba[2], rgba[3],
                       if (rgba[4] < 255) rgba[4], maxColorValue = 255)
      }, error = function(e) col)
    }, character(1), USE.NAMES = FALSE)
  }
  # Keeps the names of named vectors and lists
  to_css <- function(x) {
    if (is.null(x)) return(NULL)
    x[] <- if (is.list(x)) lapply(x, as_css_color) else as_css_color(x)
    x
  }

  # Named vectors become objects by group in JavaScript, unnamed ones arrays
  as_js_values <- function(x) {
    if (is.null(x)) return(NULL)
    if (!is.null(names(x))) return(as.list(x))
    if (length(x) == 1) return(x)
    as.list(x)
  }

  Filter(Negate(is.null), list(
    patterns = as_js_values(pattern$patterns),
    color = as_js_values(to_css(pattern$color)),
    background = as_js_values(to_css(pattern$background)),
    size = pattern$size,
    strokeWidth = pattern$strokeWidth
  ))
}

#' Set Legend for a Gene Chart
#'
#' This function configures the legend for a gene chart. It allows toggling the
#' legend's visibility, setting a background color, and assigning custom labels
#' for the legend entries. The function can also handle additional
#' customizations through various options.
#'
#' @param GC_chart The gene chart object to be modified.
#' @param group Optional; character or NULL, specifies the groups to include in
#'   the legend. If NULL, groups are taken from the 'group' attribute of the
#'   'GC_chart' object.
#' @param show Logical, specifies whether to display the legend.
#' @param backgroundColor String, the background color of the legend. Same as
#'   `style = list(backgroundColor = ...)`, which takes precedence. Default is
#'   NULL: as set by the theme, otherwise transparent.
#' @param order Optional; list, specifies the order of the legend labels.
#' @param position Character. Position of the legend, either "top" or "bottom".
#'   Default is NULL: the position of the theme, otherwise "bottom".
#' @param align Character. Horizontal alignment of the legend items, "left",
#'   "center" or "right", like the title. Each row of a horizontal legend is
#'   aligned on its own. `x` moves the legend from this position. Default is
#'   NULL: the alignment of the theme, otherwise "left".
#' @param style A list of CSS styles to be applied to the chart container. Each
#'   element of the list should be a valid CSS property-value pair. For example,
#'   list(backgroundColor = "white", border = "2px solid black"). Default is an
#'   empty list.
#' @param legendOptions List, additional options for the legend.
#' @param legendTextOptions List, additional text options for the legend.
#' @param corner_radius Numeric or NULL, radius in pixels of the rounded corners
#'   of the legend markers, e.g. `3`. Default is NULL: square markers.
#' @param ... Additional arguments to be passed to the legend configuration.
#'
#' @return Returns the modified gene chart object with the legend configured.
#'
#' @examples
#' genes_data <- data.frame(
#'   start = c(10, 90, 130, 170, 210),
#'   end = c(40, 120, 160, 200, 240),
#'   name = c('Gene 1', 'Gene 3', 'Gene 4', 'Gene 5', 'Gene 6'),
#'   group = c('A', 'B', 'B', 'A', 'C'),
#'   cluster = c('A', 'A', 'A', 'B', 'B')
#' )
#'
#' # Customize legend
#' GC_chart(genes_data, cluster = "cluster", group = "group", height = "200px") %>%
#'   GC_legend(
#'     position = "top", #bottom
#'     orientation = "horizontal", #vertical
#'     align = "center", #left / right
#'     x = 0,
#'     y = 0,
#'     adjustHeight = TRUE,
#'     backgroundColor = "#0000",
#'     order = list(),
#'     style = list(
#'       backgroundColor = "#0000"
#'       # Any other CSS styles
#'     ),
#'     legendOptions = list(
#'       cursor = "pointer",
#'       colorScheme = NULL,
#'       customColors = NULL # c("red", "green", "orange")
#'       # Additional styles
#'     ),
#'     legendTextOptions = list(
#'       cursor = "pointer",
#'       textAnchor = "start",
#'       dy = ".35em",
#'       fontSize = "12px",
#'       fontFamily = "sans-serif",
#'       fill = "black"
#'       # Additional styles
#'     )
#'   )
#'
#' @export
GC_legend <- function(
    GC_chart,
    group = NULL,
    show = TRUE,
    backgroundColor = NULL,
    order = list(),
    position = NULL,
    align = NULL,
    style = list(),
    legendOptions = list(),
    legendTextOptions = list(),
    corner_radius = NULL,
    ...
) {

  if (is.logical(group) && length(group) == 1) {
    show <- group
    group <- NULL
  }

  if (isTRUE(show) && is.null(group) && is.null(GC_chart$x$group)){
    stop("Please define a group")
  }

  if (is.null(group) && !is.null(GC_chart$x$group)){
    group <- GC_chart$x$group
  }

  if (!is.null(group) && !(group %in% names(GC_chart$x$data))) {
    stop("group column not found in data")
  }

  check_align(align)

  # The legend container takes its background from style
  if (!is.null(backgroundColor) && is.null(style$backgroundColor)) {
    style$backgroundColor <- backgroundColor
  }

  # Capture arguments and filter out NULL or empty values
  options <- Filter(function(x) !is.null(x) && length(x) > 0, list(
    group = group,
    show = show,
    order = order,
    position = position,
    align = align,
    cornerRadius = corner_radius,
    style = style,
    legendOptions = legendOptions,
    legendTextOptions = legendTextOptions,
    ...
  ))

  # Merge new options with existing options
  if (is.null(GC_chart$x$legend)) {
    GC_chart$x$legend <- options
  } else {
    GC_chart$x$legend <- modifyList(GC_chart$x$legend, options)
  }

  return(GC_chart)
}
