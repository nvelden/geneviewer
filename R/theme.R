#' Set the Theme of a GC Chart
#'
#' A theme sets the default look of every part of a chart: fonts, text and line
#' colours, the background, the gene colours and the options of each component.
#' Options set with the other `GC_` functions (e.g. `GC_labels(fontSize = )`)
#' always take precedence over the theme, whatever the order of the calls.
#'
#' Call `GC_theme()` with a `theme` to start from that theme, or without one to
#' change the current theme. The global settings (`fontFamily`, `textColor`,
#' `lineColor`, `background`, `palette`, `pattern`) are applied to all
#' components; options set for a component, in the theme or in `...`, take
#' precedence over them. `textScale` scales all text of the chart: the default font
#' sizes and those set in the theme, not those set with the other `GC_`
#' functions.
#'
#' The options of each component are those of its `GC_` function, by their
#' argument name (e.g. `normal_color`) or their name in the browser (e.g.
#' `normalColor`). Components and their functions:
#'
#' | Component | Function | Example |
#' |---|---|---|
#' | `chart` | [GC_chart()] | `list(style = list(padding = "10px"))` |
#' | `grid` | [GC_grid()] | `list(margin = list(left = "80px"))` |
#' | `cluster` | [GC_cluster()] | `list(separate_strands = TRUE)` |
#' | `title` | [GC_title()] | `list(height = 40, titleFont = list(fontSize = "18px"))` |
#' | `clusterTitle` | [GC_clusterTitle()] | `list(titleFont = list(fill = "grey"))` |
#' | `clusterFooter` | [GC_clusterFooter()] | `list(align = "center")` |
#' | `clusterLabel` | [GC_clusterLabel()] | `list(width = "120px", fontWeight = "normal")` |
#' | `sequence` | [GC_sequence()] | `list(sequenceStyle = list(stroke = "black"))` |
#' | `genes` | [GC_genes()] | `list(marker = "rbox", stroke = "none")` |
#' | `transcript` | [GC_transcript()] | `list(styleExons = list(stroke = "black"))` |
#' | `labels` | [GC_labels()] | `list(fontSize = "10px", fontStyle = "normal")` |
#' | `coordinates` | [GC_coordinates()] | `list(textStyle = list(fontSize = "8px"))` |
#' | `scale` | [GC_scale()] | `list(axis_position = "top", ticksCount = 5)` |
#' | `scaleBar` | [GC_scaleBar()] | `list(labelStyle = list(fontSize = "12px"))` |
#' | `legend` | [GC_legend()] | `list(position = "top")` |
#' | `links` | [GC_links()] | `list(normal_color = "grey", curve = FALSE)` |
#' | `tooltip` | [GC_tooltip()] | `list(backgroundColor = "black", color = "white")` |
#' | `trackMouse` | [GC_trackMouse()] | `list(cursor = "cell")` |
#' | `annotations` | [GC_annotation()] | `list(text = list(style = list(fontSize = "14px")))` |
#'
#' `annotations` takes a list per annotation type: `text`, `line`, `arrow`,
#' `textMarker`, `symbol`, `rectangle`, `promoter`, `terminator` and `arc`.
#'
#' A theme only sets how things look: it does not add components to the chart
#' (e.g. a scale needs [GC_scale()]) or change the data.
#'
#' The `"publication"` theme follows common journal figure guidelines: Arial
#' or Helvetica, 7 pt text when the chart is used at its final print size,
#' black text and keylines, visible axis ticks, no patterns or shadows, and the
#' colour-blind-safe Wong palette. Gene labels are italic, as gene symbols are
#' in journals; use `GC_labels(fontStyle = "normal")` for protein names or
#' other labels. Journals often also require axis labels with
#' units in parentheses and bold panel labels; these are content-specific and
#' must be added by the author. Export as SVG to retain
#' editable vector text and artwork, then check that fonts are embedded when
#' converting to the PDF or EPS supplied to the journal.
#'
#' @param GC_chart The GC chart object to be modified.
#' @param theme The theme to start from: the name of a built-in theme
#'   (`"default"`, `"classic"`, `"dark"`, `"vintage"`, `"infographic"`, `"publication"` or `"bw"`, see
#'   [geneviewer_theme()]) or a list of theme settings. `"bw"` is black and
#'   white: genes, exons and legend markers are filled with a pattern per
#'   group. Default is NULL: change
#'   the current theme of the chart.
#' @param textScale Text size, as a factor of the default font sizes: e.g. `1.5`
#'   makes all text one and a half times as large. Font sizes set in the theme
#'   are scaled too; those set with the other `GC_` functions are not. The
#'   title and its default height grow with the text.
#' @param fontFamily Font family of all text, e.g. `"Arial"`.
#' @param textColor Colour of all text.
#' @param lineColor Colour of the sequence line, the axis, the coordinates and
#'   the scale bar.
#' @param background Background colour of the chart, also the fill of the
#'   UTRs of transcripts.
#' @param palette Gene colours: the name of a d3 colour scheme (e.g.
#'   `"schemeSet2"`) or a geneviewer palette (e.g. `"okabeIto"`, see
#'   [geneviewer_palette()]), or a vector of colours, used in the order of the groups.
#'   Colours set with [GC_color()] or the `colorScheme` or `customColors` of
#'   [GC_genes()] take precedence.
#' @param pattern Patterns of the genes, exons and legend markers: a list with
#'   the arguments of [GC_pattern()] (`patterns`, `color`, `background`,
#'   `size`, `strokeWidth`), e.g. `list(patterns = c("dots", "grid"))`. Set
#'   with the theme, they are merged with the patterns of the current theme;
#'   [GC_pattern()] takes precedence. `FALSE` removes the patterns of the
#'   theme.
#' @param ... Options per component, as lists named after the component (see
#'   Details), e.g. `labels = list(fontSize = "10px")`.
#'
#' @return The GC chart object with the theme.
#'
#' @seealso [geneviewer_theme()] to see the settings of a built-in theme.
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
#' chart <- GC_chart(genes_data, cluster = "cluster", group = "group",
#'                   height = "200px") %>%
#'   GC_labels("name") %>%
#'   GC_clusterLabel()
#'
#' # A built-in theme
#' chart %>% GC_theme("dark")
#'
#' # A built-in theme with other fonts and colours
#' chart %>% GC_theme("classic", fontFamily = "Georgia",
#'                    palette = c("#1b9e77", "#d95f02", "#7570b3"))
#'
#' # Black and white with patterns, and other patterns
#' chart %>% GC_theme("bw")
#' chart %>% GC_theme("bw", pattern = list(patterns = c("dots", "crosshatch", "none")))
#'
#' # Larger text, e.g. for a slide or a figure that is printed small
#' chart %>% GC_theme("classic", textScale = 1.4)
#'
#' # Change one component; options of the GC_ functions still take precedence
#' chart %>%
#'   GC_theme("classic", labels = list(fontSize = "14px", fill = "grey")) %>%
#'   GC_labels("name", fontSize = "10px")
#'
#' @export
GC_theme <- function(
    GC_chart,
    theme = NULL,
    textScale = NULL,
    fontFamily = NULL,
    textColor = NULL,
    lineColor = NULL,
    background = NULL,
    palette = NULL,
    pattern = NULL,
    ...
) {

  if (is.null(theme)) {
    base <- if (is.null(GC_chart$x$theme)) list() else GC_chart$x$theme
  } else {
    base <- resolve_theme(theme)
  }

  settings <- c(
    Filter(Negate(is.null), list(
      textScale = textScale,
      fontFamily = fontFamily,
      textColor = textColor,
      lineColor = lineColor,
      background = background,
      palette = palette,
      pattern = if (!isFALSE(pattern)) pattern
    )),
    list(...)
  )
  check_theme(settings)

  GC_chart$x$theme <- merge_theme_settings(base, settings)
  if (isFALSE(pattern)) {
    GC_chart$x$theme$pattern <- NULL
  }

  return(GC_chart)
}

#' Settings of a Built-in Theme
#'
#' Returns the settings of a built-in theme of [GC_theme()], to see them or to
#' start a theme of your own.
#'
#' @param name Name of the theme: `"default"`, `"classic"`,
#'   `"dark"`, `"vintage"`, `"infographic"`, `"publication"` or `"bw"`.
#'
#' @return A list of theme settings, to use as the `theme` of [GC_theme()].
#'
#' @examples
#' geneviewer_theme("dark")
#'
#' # A theme of your own, based on "classic"
#' my_theme <- utils::modifyList(geneviewer_theme("classic"),
#'                               list(textColor = "navy"))
#'
#' @export
geneviewer_theme <- function(name = "default") {
  if (!is.character(name) || length(name) != 1 || !name %in% names(builtin_themes)) {
    stop("name must be one of: ", paste0('"', names(builtin_themes), '"', collapse = ", "))
  }
  builtin_themes[[name]]
}

#' Colours of a geneviewer Palette
#'
#' Besides the d3 colour schemes (e.g. `"schemeSet2"`), the gene colours can be
#' set by the name of a geneviewer palette, in [GC_theme()] (`palette`) and
#' in the `colorScheme` of [GC_genes()], [GC_transcript()] and [GC_color()]:
#'
#' - `"okabeIto"`: 8 colours that stay distinct with colour blindness
#'   (Okabe & Ito, also known as the Wong palette), the palette of the
#'   `"publication"` theme.
#' - `"classic"`: 9 clear colours, the palette of the `"classic"` theme.
#' - `"vivid"`: 9 bright colours for dark backgrounds.
#' - `"dark"`: 11 soft colours for dark backgrounds, the palette of the
#'   `"dark"` theme.
#' - `"vintage"`: 10 muted colours, the palette of the `"vintage"` theme.
#' - `"infographic"`: 15 strong colours, the palette of the `"infographic"`
#'   theme.
#' - `"greys"`: 6 greys, from dark to light.
#' - `"blues"`: 7 blues, from dark to light.
#'
#' @param name Name of the palette. Default is NULL: the names of all
#'   palettes.
#'
#' @return The colours of the palette, or the names of all palettes.
#'
#' @examples
#' geneviewer_palette("okabeIto")
#' geneviewer_palette()
#'
#' genes_data <- data.frame(
#'   start = c(10, 90, 130, 170, 210),
#'   end = c(40, 120, 160, 200, 240),
#'   group = c('A', 'B', 'B', 'A', 'C')
#' )
#' GC_chart(genes_data, group = "group", height = "100px") %>%
#'   GC_genes(colorScheme = "okabeIto")
#'
#' @export
geneviewer_palette <- function(name = NULL) {
  if (is.null(name)) {
    return(names(geneviewer_palettes))
  }
  if (!is.character(name) || length(name) != 1 || !name %in% names(geneviewer_palettes)) {
    stop("name must be one of: ", paste0('"', names(geneviewer_palettes), '"', collapse = ", "))
  }
  geneviewer_palettes[[name]]
}

geneviewer_palettes <- list(
  okabeIto = c("#E69F00", "#56B4E9", "#009E73", "#F0E442",
               "#0072B2", "#D55E00", "#CC79A7", "#999999"),
  classic = c("#5470c6", "#91cc75", "#fac858", "#ee6666", "#73c0de",
              "#3ba272", "#fc8452", "#9a60b4", "#ea7ccc"),
  vivid = c("#4992ff", "#7cffb2", "#fddd60", "#ff6e76", "#58d9f9",
            "#05c091", "#ff8a45", "#8d48e3", "#dd79ff"),
  dark = c("#dd6b66", "#759aa0", "#e69d87", "#8dc1a9", "#ea7e53", "#eedd78",
           "#73a373", "#73b9bc", "#7289ab", "#91ca8c", "#f49f42"),
  vintage = c("#d87c7c", "#919e8b", "#d7ab82", "#6e7074", "#61a0a8",
              "#efa18d", "#787464", "#cc7e63", "#724e58", "#4b565b"),
  infographic = c("#C1232B", "#27727B", "#FCCE10", "#E87C25", "#B5C334",
                  "#FE8463", "#9BCA63", "#FAD860", "#F3A43B", "#60C0DD",
                  "#D7504B", "#C6E579", "#F4E001", "#F0805A", "#26C0C0"),
  greys = c("#252525", "#525252", "#737373", "#969696", "#bdbdbd", "#d9d9d9"),
  blues = c("#08306b", "#08519c", "#2171b5", "#4292c6", "#6baed6", "#9ecae1", "#c6dbef")
)

# Built-in themes, as settings for GC_theme()

# The default tooltip of ECharts, with 12px text like the rest of the chart
# (ECharts uses 14px and 10px padding)
echarts_tooltip <- list(backgroundColor = "#ffffff", color = "#6d6e73",
                        border = "1px solid #b7b9be", borderRadius = "4px",
                        fontSize = "12px", fontFamily = "sans-serif", padding = "8px",
                        boxShadow = "1px 2px 10px rgba(0, 0, 0, 0.2)")


builtin_themes <- list(
  default = list(),
  # The default theme of ECharts 5: only the genes and links are coloured. The
  # title colour (#464646) for titles, cluster labels and the legend, the
  # subtitle colour (#6E7079) for the other text and all lines; sans-serif
  # 12px text, an 18px title, 5px axis ticks and the ECharts tooltip
  classic = list(
    fontFamily = "sans-serif",
    textColor = "#6E7079",
    lineColor = "#6E7079",
    palette = "classic",
    title = list(titleFont = list(fontSize = "18px", fill = "#464646"),
                 subtitleFont = list(fontSize = "12px")),
    clusterTitle = list(titleFont = list(fill = "#464646")),
    clusterLabel = list(fill = "#464646"),
    genes = list(stroke = "none", corner_radius = 3),
    transcript = list(styleExons = list(strokeWidth = 0, cornerRadius = 3),
                      styleUTRs = list(cornerRadius = 3)),
    labels = list(fontStyle = "normal", fontSize = "12px"),
    # Coordinates keep 10px: at 12px the numbers of adjacent genes overlap
    coordinates = list(tickStyle = list(lineLength = 5)),
    scale = list(textStyle = list(fontSize = "12px"), tickStyle = list(lineLength = 5)),
    scaleBar = list(labelStyle = list(fontSize = "12px")),
    legend = list(corner_radius = 3, legendTextOptions = list(fill = "#464646")),
    # Links in the blue and red of the palette
    links = list(normal_color = "#5470c6", inverted_color = "#ee6666",
                 linkStyle = list(fillOpacity = 0.6),
                 colorBarOptions = list(barOptions = list(stroke = "none"),
                                        labelOptions = list(fontSize = 12),
                                        titleOptions = list(fontSize = 12))),
    tooltip = echarts_tooltip,
    trackMouse = echarts_tooltip
  ),
  # The ECharts dark theme (ECharts 4) on the ECharts defaults: #eee for all
  # text and lines but the subtitle (#aaa), sans-serif 12px text, an 18px
  # title, 5px axis ticks, links in the colours of the theme fading into the
  # background, and the light ECharts tooltip
  dark = list(
    fontFamily = "sans-serif",
    background = "#333333",
    textColor = "#eeeeee",
    lineColor = "#eeeeee",
    palette = "dark",
    title = list(titleFont = list(fontSize = "18px"),
                 subtitleFont = list(fontSize = "12px", fill = "#aaaaaa")),
    clusterTitle = list(subtitleFont = list(fill = "#aaaaaa")),
    clusterLabel = list(subtitleFont = list(fill = "#aaaaaa")),
    genes = list(stroke = "none", corner_radius = 3),
    transcript = list(styleExons = list(strokeWidth = 0, cornerRadius = 3),
                      styleUTRs = list(cornerRadius = 3)),
    labels = list(fontStyle = "normal", fontSize = "12px"),
    # Coordinates keep 10px: at 12px the numbers of adjacent genes overlap
    coordinates = list(tickStyle = list(lineLength = 5)),
    scale = list(textStyle = list(fontSize = "12px"), tickStyle = list(lineLength = 5)),
    scaleBar = list(labelStyle = list(fontSize = "12px")),
    legend = list(corner_radius = 3),
    links = list(normal_color = "#759aa0", inverted_color = "#dd6b66",
                 low_color = "#333333", linkStyle = list(fillOpacity = 0.6),
                 colorBarOptions = list(barOptions = list(stroke = "none"),
                                        labelOptions = list(fontSize = 12),
                                        titleOptions = list(fontSize = 12))),
    tooltip = echarts_tooltip,
    trackMouse = echarts_tooltip
  ),
  # The colours and flat style of the ECharts vintage theme: its palette and
  # background, with the text and axis colours of the ECharts defaults
  vintage = list(
    background = "#fef8ef",
    textColor = "#3f3a33",
    lineColor = "#787464",
    palette = "vintage",
    title = list(subtitleFont = list(fill = "#787464")),
    clusterTitle = list(subtitleFont = list(fill = "#787464")),
    clusterLabel = list(subtitleFont = list(fill = "#787464")),
    sequence = list(sequenceStyle = list(stroke = "#c9c2b5")),
    genes = list(stroke = "none", corner_radius = 3),
    transcript = list(styleExons = list(strokeWidth = 0, cornerRadius = 3),
                      styleUTRs = list(cornerRadius = 3)),
    labels = list(fontStyle = "normal", fontSize = "11px"),
    legend = list(corner_radius = 3),
    links = list(normal_color = "#61a0a8", inverted_color = "#d87c7c",
                 low_color = "#fef8ef", linkStyle = list(fillOpacity = 0.6),
                 colorBarOptions = list(barOptions = list(stroke = "none"))),
    tooltip = echarts_tooltip,
    trackMouse = echarts_tooltip
  ),
  # The ECharts infographic theme on the ECharts defaults: only the genes and
  # links are coloured. The title colour (#464646) for titles, cluster labels and the
  # legend, the subtitle colour (#6E7079) for the other text and all lines;
  # sans-serif 12px text, an 18px title, 5px axis ticks and dark tooltips
  infographic = list(
    fontFamily = "sans-serif",
    textColor = "#6E7079",
    lineColor = "#6E7079",
    palette = "infographic",
    title = list(titleFont = list(fontSize = "18px", fill = "#464646"),
                 subtitleFont = list(fontSize = "12px")),
    clusterTitle = list(titleFont = list(fill = "#464646")),
    clusterLabel = list(fill = "#464646"),
    genes = list(stroke = "none", corner_radius = 3),
    transcript = list(styleExons = list(strokeWidth = 0, cornerRadius = 3),
                      styleUTRs = list(cornerRadius = 3)),
    labels = list(fontStyle = "normal", fontSize = "12px"),
    # Coordinates keep 10px: at 12px the numbers of adjacent genes overlap
    coordinates = list(tickStyle = list(lineLength = 5)),
    scale = list(textStyle = list(fontSize = "12px"), tickStyle = list(lineLength = 5)),
    scaleBar = list(labelStyle = list(fontSize = "12px")),
    legend = list(corner_radius = 3, legendTextOptions = list(fill = "#464646")),
    # Links in the teal and red of the theme
    links = list(normal_color = "#27727B", inverted_color = "#C1232B",
                 linkStyle = list(fillOpacity = 0.6),
                 colorBarOptions = list(barOptions = list(stroke = "none"),
                                        labelOptions = list(fontSize = 12),
                                        titleOptions = list(fontSize = 12))),
    tooltip = list(backgroundColor = "rgba(50, 50, 50, 0.85)", color = "#ffffff",
                   border = "none", borderRadius = "4px", fontSize = "12px"),
    trackMouse = list(backgroundColor = "rgba(50, 50, 50, 0.85)", color = "#ffffff",
                      border = "none", borderRadius = "4px", fontSize = "12px")
  ),
  # Figures for journals, after common journal figure guidelines, in the style
  # of the other themes: Arial or Helvetica, text of 7 pt (9.33px; at most 7
  # and at least 5 pt in print), black text and keylines, italic gene labels,
  # black axis lines with ticks, no gridlines, patterns or shadows, and the colour-blind-safe colours
  # of Wong (2011), with links in its blue and vermillion. Journals reserve
  # 8 pt bold text for panel labels, which authors add separately. Sizes are
  # for a chart drawn at its final print size.
  publication = list(
    fontFamily = "Arial, Helvetica, sans-serif",
    textColor = "black",
    lineColor = "black",
    palette = "okabeIto",
    title = list(titleFont = list(fontSize = "9.33px", fill = "black"),
                 subtitleFont = list(fontSize = "9.33px")),
    clusterTitle = list(titleFont = list(fontSize = "9.33px"), subtitleFont = list(fontSize = "9.33px")),
    clusterFooter = list(titleFont = list(fontSize = "9.33px"), subtitleFont = list(fontSize = "9.33px")),
    clusterLabel = list(fontSize = "9.33px", subtitleFont = list(fontSize = "9.33px")),
    genes = list(stroke = "black", strokeWidth = 0.75, corner_radius = 3),
    # No keylines on exons and UTRs: on a short exon the outline hides its colour
    transcript = list(styleExons = list(strokeWidth = 0, cornerRadius = 3),
                      styleUTRs = list(cornerRadius = 3, fontSize = "9.33px"),
                      labelOptions = list(fontSize = "9.33px")),
    # Gene symbols are italic in journals
    labels = list(fontStyle = "italic", fontSize = "9.33px"),
    coordinates = list(textStyle = list(fontSize = "9.33px"), tickStyle = list(lineLength = 4)),
    scale = list(textStyle = list(fontSize = "9.33px"), tickStyle = list(lineLength = 4)),
    scaleBar = list(labelStyle = list(fontSize = "9.33px")),
    legend = list(corner_radius = 2,
                  legendOptions = list(stroke = "black", strokeWidth = 0.75),
                  legendTextOptions = list(fontSize = "9.33px")),
    links = list(normal_color = "#0072B2", inverted_color = "#D55E00",
                 linkStyle = list(fillOpacity = 0.6), labelStyle = list(fontSize = "9.33px"),
                 colorBarOptions = list(barOptions = list(stroke = "none"),
                                        labelOptions = list(fontSize = 9.33),
                                        titleOptions = list(fontSize = 9.33))),
    annotations = list(text = list(style = list(fontSize = "9.33px")),
                       textMarker = list(textStyle = list(fontSize = "9.33px")),
                       arc = list(textStyle = list(fontSize = "9.33px"))),
    tooltip = list(backgroundColor = "white", color = "black",
                   border = "1px solid black", borderRadius = "0",
                   boxShadow = "none", fontSize = "9.33px", padding = "8px"),
    trackMouse = list(backgroundColor = "white", color = "black",
                      border = "1px solid black", borderRadius = "0",
                      boxShadow = "none", fontSize = "9.33px", padding = "8px")
  ),
  # Black and white, in the style of the other themes: black for titles,
  # cluster labels and the legend, grey (#555555) for the other text and all
  # lines; sans-serif 12px text, an 18px title, 5px axis ticks, rounded genes
  # and legend markers with a black outline around the patterns, grey links
  # and the ECharts tooltip
  bw = list(
    fontFamily = "sans-serif",
    textColor = "#555555",
    lineColor = "#555555",
    pattern = list(color = "black", background = "white", size = 6, strokeWidth = 1),
    title = list(titleFont = list(fontSize = "18px", fill = "black"),
                 subtitleFont = list(fontSize = "12px")),
    clusterTitle = list(titleFont = list(fill = "black")),
    clusterLabel = list(fill = "black"),
    genes = list(stroke = "black", strokeWidth = 0.75, corner_radius = 3),
    transcript = list(styleExons = list(stroke = "black", strokeWidth = 0.75, cornerRadius = 3),
                      styleUTRs = list(cornerRadius = 3)),
    labels = list(fontStyle = "normal", fontSize = "12px"),
    # Coordinates keep 10px: at 12px the numbers of adjacent genes overlap
    coordinates = list(tickStyle = list(lineLength = 5)),
    scale = list(textStyle = list(fontSize = "12px"), tickStyle = list(lineLength = 5)),
    scaleBar = list(labelStyle = list(fontSize = "12px")),
    legend = list(corner_radius = 3, legendTextOptions = list(fill = "black")),
    links = list(normal_color = "#bdbdbd", inverted_color = "#525252",
                 colorBarOptions = list(barOptions = list(stroke = "#555555"),
                                        labelOptions = list(fontSize = 12),
                                        titleOptions = list(fontSize = 12))),
    tooltip = echarts_tooltip,
    trackMouse = echarts_tooltip
  )
)

# Global settings and components of a theme, with the key of each component in
# the browser (geneviewer.js)
theme_global_settings <- c("textScale", "fontFamily", "textColor", "lineColor", "background", "palette", "pattern")

theme_components <- c(
  chart = "chartOptions",
  grid = "containerOptions",
  cluster = "clusterOptions",
  title = "titleOptions",
  clusterTitle = "clusterTitleOptions",
  clusterFooter = "footerOptions",
  clusterLabel = "clusterLabelOptions",
  sequence = "sequenceOptions",
  genes = "geneOptions",
  transcript = "transcriptOptions",
  labels = "labelsOptions",
  coordinates = "coordinatesOptions",
  scale = "scaleOptions",
  scaleBar = "scaleBarOptions",
  legend = "legendOptions",
  links = "linkOptions",
  tooltip = "tooltipOptions",
  trackMouse = "trackMouseOptions"
)

theme_annotation_types <- c(
  text = "textAnnotationOptions",
  line = "lineAnnotationOptions",
  arrow = "arrowAnnotationOptions",
  textMarker = "textMarkerAnnotationOptions",
  symbol = "symbolAnnotationOptions",
  rectangle = "rectangleAnnotationOptions",
  promoter = "promoterAnnotationOptions",
  terminator = "terminatorAnnotationOptions",
  arc = "curveAnnotationOptions"
)

resolve_theme <- function(theme) {
  if (is.character(theme)) {
    return(geneviewer_theme(theme))
  }
  if (!is.list(theme)) {
    stop("theme must be the name of a built-in theme or a list of theme settings")
  }
  check_theme(theme)
  theme
}

check_theme <- function(settings) {
  if (length(settings) == 0) {
    return(invisible())
  }
  valid <- c(theme_global_settings, names(theme_components), "annotations")
  if (is.null(names(settings)) || any(names(settings) == "")) {
    stop("All theme settings must be named")
  }
  unknown <- setdiff(names(settings), valid)
  if (length(unknown) > 0) {
    stop("Unknown theme setting: ", paste(unknown, collapse = ", "),
         ". Use one of: ", paste(valid, collapse = ", "))
  }
  components <- settings[intersect(names(settings), c(names(theme_components), "annotations"))]
  for (name in names(components)) {
    # NULL removes the component from the theme
    if (!is.null(components[[name]]) && !is.list(components[[name]])) {
      stop("Theme setting '", name, "' must be a list of options")
    }
  }
  if (!is.null(settings$textScale) &&
      (!is.numeric(settings$textScale) || length(settings$textScale) != 1 ||
       is.na(settings$textScale) || settings$textScale <= 0)) {
    stop("textScale must be a single positive number")
  }
  for (name in intersect(names(settings), setdiff(theme_global_settings, c("textScale", "palette", "pattern")))) {
    if (!is.character(settings[[name]]) || length(settings[[name]]) != 1) {
      stop("Theme setting '", name, "' must be a single character string")
    }
  }
  if (!is.null(settings$palette) && !is.character(settings$palette)) {
    stop("palette must be the name of a d3 colour scheme or a character vector of colours")
  }
  if (!is.null(settings$pattern)) {
    pattern_args <- c("patterns", "color", "background", "size", "strokeWidth")
    if (!is.list(settings$pattern) || is.null(names(settings$pattern)) ||
        !all(names(settings$pattern) %in% pattern_args)) {
      stop("pattern must be a list with the arguments of GC_pattern(): ",
           paste(pattern_args, collapse = ", "))
    }
    pattern_to_js(settings$pattern)
  }
  unknown <- setdiff(names(settings$annotations), names(theme_annotation_types))
  if (length(unknown) > 0) {
    stop("Unknown annotation type in theme: ", paste(unknown, collapse = ", "),
         ". Use one of: ", paste(names(theme_annotation_types), collapse = ", "))
  }
  invisible()
}

# Merges theme settings: named lists are merged at every level, other values
# (unnamed lists such as itemStyle or a palette included) replace the old value,
# and NULL removes it. utils::modifyList() ignores unnamed lists.
merge_theme_settings <- function(x, y) {
  is_named_list <- function(z) is.list(z) && !is.null(names(z))
  for (name in names(y)) {
    if (is.null(y[[name]])) {
      x[[name]] <- NULL
    } else if (is_named_list(x[[name]]) && is_named_list(y[[name]])) {
      x[[name]] <- merge_theme_settings(x[[name]], y[[name]])
    } else {
      x[[name]] <- y[[name]]
    }
  }
  x
}

# Option names of the GC_ functions (normal_color) as in the browser (normalColor)
snake_to_camel <- function(x) {
  gsub("_([a-z])", "\\U\\1", x, perl = TRUE)
}

camel_case_names <- function(options) {
  if (length(options) > 0 && !is.null(names(options))) {
    names(options) <- snake_to_camel(names(options))
  }
  options
}

# Options of each component set by the global settings
theme_global_options <- function(theme) {
  font <- Filter(Negate(is.null), list(fontFamily = theme$fontFamily))
  text <- c(font, Filter(Negate(is.null), list(fill = theme$textColor)))
  html_text <- c(font, Filter(Negate(is.null), list(color = theme$textColor)))
  line <- Filter(Negate(is.null), list(stroke = theme$lineColor))

  # A d3 scheme or palette name, or colours (a list, so one colour is still an
  # array). Palette names are replaced by their colours by render_theme().
  palette <- theme$palette
  if (!is.null(palette) && !(length(palette) == 1 &&
                             (grepl("^scheme", palette) || palette %in% names(geneviewer_palettes)))) {
    palette <- as.list(palette)
  }
  colors <- Filter(Negate(is.null), list(colorScheme = palette))
  pattern <- if (!is.null(theme$pattern)) list(pattern = pattern_to_js(theme$pattern))

  options <- list(
    chart = list(style = Filter(Negate(is.null), list(backgroundColor = theme$background))),
    title = list(titleFont = text, subtitleFont = text),
    clusterTitle = list(titleFont = text, subtitleFont = text),
    clusterFooter = list(titleFont = text, subtitleFont = text),
    clusterLabel = c(text, list(subtitleFont = text)),
    sequence = list(sequenceStyle = line, markerStyle = line),
    genes = c(colors, pattern),
    # UTRs are open boxes, filled with the background
    transcript = c(colors, pattern, list(labelOptions = html_text),
                   list(styleUTRs = Filter(Negate(is.null), list(fill = theme$background)))),
    labels = text,
    coordinates = list(textStyle = text, tickStyle = line),
    scale = list(textStyle = text, tickStyle = line, lineStyle = line),
    scaleBar = list(labelStyle = text, scaleBarLineStyle = line, scaleBarTickStyle = line),
    legend = list(legendOptions = c(colors, pattern), legendTextOptions = text),
    links = list(labelStyle = text,
                 colorBarOptions = list(labelOptions = text, titleOptions = text)),
    tooltip = html_text,
    trackMouse = html_text,
    annotations = list(text = list(style = text), textMarker = list(textStyle = text),
                       arc = list(textStyle = text))
  )
  drop_empty(options)
}

# Removes empty lists, so they do not replace defaults in the browser
drop_empty <- function(x) {
  if (!is.list(x)) {
    return(x)
  }
  x <- lapply(x, drop_empty)
  x[!vapply(x, function(y) is.list(y) && length(y) == 0, logical(1))]
}

# preRenderHook of GC_chart(): the theme is kept as set with GC_theme() and sent
# to the browser in its form there when the chart is drawn
render_theme <- function(widget) {
  # The widget width can change after GC_chart(), e.g. through GC_grid().
  # Shiny's output element otherwise keeps only GC_chartOutput()'s width.
  widget$x$size <- list(width = widget$width)
  widget$x$theme <- theme_to_js(widget$x$theme)
  widget$x <- resolve_palette_names(widget$x)
  widget
}

# Replaces each colorScheme that is the name of a geneviewer palette by its
# colours; d3 scheme names are left for the browser
resolve_palette_names <- function(x) {
  if (!is.list(x)) {
    return(x)
  }
  for (i in seq_along(x)) {
    name <- names(x)[i]
    value <- x[[i]]
    if (identical(name, "colorScheme") && is.character(value) && length(value) == 1 &&
        value %in% names(geneviewer_palettes)) {
      x[[i]] <- as.list(geneviewer_palettes[[value]])
    } else if (is.list(value)) {
      x[[i]] <- resolve_palette_names(value)
    }
  }
  x
}

# The theme as sent to the browser: the options of each component, under the
# key used in geneviewer.js, with the global settings applied
theme_to_js <- function(theme) {
  if (is.null(theme) || length(theme) == 0) {
    return(NULL)
  }

  components <- lapply(theme[intersect(names(theme), names(theme_components))], camel_case_names)
  if (!is.null(theme$annotations)) {
    components$annotations <- lapply(theme$annotations, camel_case_names)
  }

  # Options of a component take precedence over the global settings
  options <- merge_theme_settings(theme_global_options(theme), components)

  js <- list()
  for (name in intersect(names(theme_components), names(options))) {
    js[[theme_components[[name]]]] <- options[[name]]
  }
  for (type in names(options$annotations)) {
    js[[theme_annotation_types[[type]]]] <- options$annotations[[type]]
  }
  # Scales the font sizes in the browser, where the defaults are known
  js$textScale <- theme$textScale
  js
}
