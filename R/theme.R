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
#' @param GC_chart The GC chart object to be modified.
#' @param theme The theme to start from: the name of a built-in theme
#'   (`"default"`, `"minimal"`, `"classic"`, `"dark"` or `"bw"`, see
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
#' @param background Background colour of the chart.
#' @param palette Gene colours: the name of a d3 colour scheme (e.g.
#'   `"schemeSet2"`) or a vector of colours, used in the order of the groups.
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
#'   GC_theme("minimal", labels = list(fontSize = "14px", fill = "grey")) %>%
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
#' @param name Name of the theme: `"default"`, `"minimal"`, `"classic"`,
#'   `"dark"` or `"bw"`.
#'
#' @return A list of theme settings, to use as the `theme` of [GC_theme()].
#'
#' @examples
#' geneviewer_theme("dark")
#'
#' # A theme of your own, based on "minimal"
#' my_theme <- utils::modifyList(geneviewer_theme("minimal"),
#'                               list(textColor = "navy"))
#'
#' @export
geneviewer_theme <- function(name = "default") {
  if (!is.character(name) || length(name) != 1 || !name %in% names(builtin_themes)) {
    stop("name must be one of: ", paste0('"', names(builtin_themes), '"', collapse = ", "))
  }
  builtin_themes[[name]]
}

# Built-in themes, as settings for GC_theme()
okabe_ito <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442",
               "#0072B2", "#D55E00", "#CC79A7", "#999999")

builtin_themes <- list(
  default = list(),
  minimal = list(
    textColor = "#444444",
    lineColor = "#bdbdbd",
    # A thin outline in the line colour, so white genes (e.g. "No Hit") stay visible
    genes = list(stroke = "#bdbdbd", strokeWidth = 0.5),
    transcript = list(styleExons = list(stroke = "#bdbdbd", strokeWidth = 0.5)),
    labels = list(fontStyle = "normal", fontSize = "11px"),
    clusterLabel = list(fontWeight = "normal"),
    links = list(linkStyle = list(fillOpacity = 0.5))
  ),
  classic = list(
    fontFamily = "Arial, Helvetica, sans-serif",
    textColor = "black",
    lineColor = "black",
    palette = okabe_ito,
    genes = list(stroke = "black", strokeWidth = 0.75)
  ),
  # The colours and flat style of the ECharts dark theme
  dark = list(
    background = "#100C2A",
    textColor = "#B9B8CE",
    lineColor = "#B9B8CE",
    palette = c("#4992ff", "#7cffb2", "#fddd60", "#ff6e76", "#58d9f9",
                "#05c091", "#ff8a45", "#8d48e3", "#dd79ff"),
    title = list(titleFont = list(fill = "#EEF1FA")),
    clusterTitle = list(titleFont = list(fill = "#EEF1FA")),
    clusterLabel = list(fontWeight = "normal"),
    sequence = list(sequenceStyle = list(stroke = "#484753")),
    genes = list(stroke = "none"),
    transcript = list(styleExons = list(strokeWidth = 0)),
    labels = list(fontStyle = "normal", fontSize = "11px"),
    legend = list(legendOptions = list(rx = "3px")),
    links = list(normal_color = "#4992ff", inverted_color = "#ff6e76",
                 low_color = "#100C2A", linkStyle = list(fillOpacity = 0.6),
                 colorBarOptions = list(barOptions = list(stroke = "none"))),
    tooltip = list(backgroundColor = "rgba(53, 52, 80, 0.95)",
                   border = "1px solid #71708A"),
    trackMouse = list(backgroundColor = "rgba(53, 52, 80, 0.95)",
                      border = "1px solid #71708A")
  ),
  bw = list(
    fontFamily = "Arial, Helvetica, sans-serif",
    textColor = "black",
    lineColor = "black",
    pattern = list(color = "black", background = "white", size = 6, strokeWidth = 1),
    genes = list(stroke = "black", strokeWidth = 0.75),
    transcript = list(styleExons = list(stroke = "black", strokeWidth = 0.75)),
    links = list(normal_color = "#bdbdbd", inverted_color = "#525252",
                 colorBarOptions = list(barOptions = list(stroke = "black")))
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

  # A d3 scheme name, or colours (a list, so one colour is still an array)
  palette <- theme$palette
  if (!is.null(palette) && !(length(palette) == 1 && grepl("^scheme", palette))) {
    palette <- as.list(palette)
  }
  colors <- Filter(Negate(is.null), list(colorScheme = palette))
  pattern <- if (!is.null(theme$pattern)) list(pattern = pattern_to_js(theme$pattern))

  options <- list(
    chart = list(style = Filter(Negate(is.null), list(backgroundColor = theme$background))),
    title = list(titleFont = text, subtitleFont = text),
    clusterTitle = list(titleFont = text, subtitleFont = text),
    clusterFooter = list(titleFont = text, subtitleFont = text),
    clusterLabel = text,
    sequence = list(sequenceStyle = line, markerStyle = line),
    genes = c(colors, pattern),
    transcript = c(colors, pattern, list(labelOptions = html_text)),
    labels = text,
    coordinates = list(textStyle = text, tickStyle = line),
    scale = list(textStyle = text, tickStyle = line, lineStyle = line),
    scaleBar = list(labelStyle = text, scaleBarLineStyle = line, scaleBarTickStyle = line),
    legend = list(legendOptions = c(colors, pattern), legendTextOptions = text),
    links = list(labelStyle = text,
                 colorBarOptions = list(labelOptions = text, titleOptions = text)),
    tooltip = html_text,
    trackMouse = html_text,
    annotations = list(text = list(style = text), textMarker = list(textStyle = text))
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
  widget$x$theme <- theme_to_js(widget$x$theme)
  widget
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
