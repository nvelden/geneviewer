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
#' | `labels` | [GC_labels()] | `list(fontSize = "10px", leaderLine = list(stroke = "grey"))` |
#' | `coordinates` | [GC_coordinates()] | `list(textStyle = list(fontSize = "8px"))` |
#' | `scale` | [GC_scale()] | `list(axis_position = "top", ticksCount = 5)` |
#' | `scaleBar` | [GC_scaleBar()] | `list(labelStyle = list(fontSize = "12px"))` |
#' | `legend` | [GC_legend()] | `list(position = "top")` |
#' | `links` | [GC_links()] | `list(normal_color = "grey", curve = FALSE)` |
#' | `tooltip` | [GC_tooltip()] | `list(backgroundColor = "black", color = "white")` |
#' | `trackMouse` | [GC_trackMouse()] | `list(cursor = "cell")` |
#' | `toolbar` | [GC_toolbar()] | `list(color = "#9e9e9e", hoverColor = "black")` |
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
#' @param lineColor Colour of the sequence line, the axis, the coordinates,
#'   the scale bar and the lines from spread labels to their genes
#'   ([GC_labels()]`(adjustLabels = "spread")`).
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

  spec <- spec_args(match.call(), environment())
  if (!is.null(theme)) {
    resolve_theme(theme)
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

  add_spec_theme(GC_chart, spec)
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
  themes <- builtin_themes()
  if (!is.character(name) || length(name) != 1 || !name %in% names(themes)) {
    stop("name must be one of: ", paste0('"', names(themes), '"', collapse = ", "))
  }
  themes[[name]]
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
  palettes <- geneviewer_palettes()
  if (is.null(name)) {
    return(names(palettes))
  }
  if (!is.character(name) || length(name) != 1 || !name %in% names(palettes)) {
    stop("name must be one of: ", paste0('"', names(palettes), '"', collapse = ", "))
  }
  palettes[[name]]
}

# The built-in themes and palettes, from the themes.json of the vendored
# JavaScript library (with the option names of the GC_ functions), read once
theme_store <- new.env(parent = emptyenv())

themes_json <- function() {
  if (is.null(theme_store$json)) {
    lib <- system.file("htmlwidgets", "lib", package = "geneviewer")
    folder <- list.files(lib, pattern = "^geneviewer-", full.names = TRUE)
    file <- file.path(folder, "themes.json")
    if (length(file) != 1 || !file.exists(file)) {
      stop("The themes of the JavaScript library are missing (themes.json).")
    }
    theme_store$json <- as_r_values(jsonlite::read_json(file, simplifyVector = FALSE))
  }
  theme_store$json
}

# Numbers as doubles, as they are written in R; an empty object is list()
as_r_values <- function(x) {
  if (is.list(x)) {
    if (length(x) == 0) return(list())
    x[] <- lapply(x, as_r_values)
    return(x)
  }
  if (is.integer(x)) as.double(x) else x
}

#' The built-in themes, by name
#' @noRd
builtin_themes <- function() {
  themes_json()$themes
}

#' The geneviewer palettes, by name: character vectors of colours
#' @noRd
geneviewer_palettes <- function() {
  lapply(themes_json()$palettes, unlist)
}

# Global settings and components of a theme
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
  trackMouse = "trackMouseOptions",
  toolbar = "toolbarOptions"
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
    check_pattern(settings$pattern)
  }
  if (!is.null(settings$toolbar)) check_toolbar(camel_case_names(settings$toolbar))
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
