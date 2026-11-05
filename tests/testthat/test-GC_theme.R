chart <- GC_chart(ophA_clusters, cluster = "cluster", group = "class", height = "200px")

# The theme as the library sends it to the renderer: the options of each
# component under its key there, with the global settings applied
theme_js <- function(theme, simplify = TRUE) {
  compile_chart(GC_theme(chart, theme), simplify = simplify)$theme
}

test_that("GC_theme() starts from a built-in theme and changes the current theme", {
  themed <- GC_theme(chart, "dark")
  expect_equal(themed$x$spec$theme, list(base = "dark"))
  expect_equal(compile_chart(themed)$theme, theme_js(geneviewer_theme("dark")))

  # Without a theme, the current theme is changed
  themed <- GC_theme(themed, textColor = "white", labels = list(fontSize = "9px"))
  expect_equal(themed$x$spec$theme,
               list(base = "dark", textColor = "white", labels = list(fontSize = "9px")))
  js <- compile_chart(themed)$theme
  expect_equal(js$titleOptions$titleFont$fill, "white")
  expect_equal(js$chartOptions$style$backgroundColor, "#333333")
  expect_equal(js$labelsOptions$fontSize, "9px")

  # Nested options are merged, NULL removes a component
  themed <- GC_theme(themed, links = list(linkStyle = list(stroke = "red")))
  expect_equal(compile_chart(themed)$theme$linkOptions$linkStyle, list(fillOpacity = 0.6, stroke = "red"))
  themed <- GC_theme(themed, links = NULL)
  expect_true("links" %in% names(themed$x$spec$theme))
  expect_null(themed$x$spec$theme$links)
  expect_match(as.character(htmlwidgets:::toJSON(themed$x$spec$theme)), '"links":null')
  expect_null(compile_chart(themed)$theme$linkOptions$linkStyle)
  expect_null(compile_chart(GC_theme(chart, "dark", links = NULL))$theme$linkOptions$linkStyle)

  # A new theme replaces the current one
  expect_equal(GC_theme(themed, "default")$x$spec$theme, list(base = "default"))
  expect_null(compile_chart(GC_theme(themed, "default"))$theme)
})

test_that("GC_theme() checks its settings", {
  expect_error(GC_theme(chart, "unknown"), "must be one of")
  expect_error(GC_theme(chart, gene = list()), "Unknown theme setting: gene")
  expect_error(GC_theme(chart, labels = "10px"), "must be a list")
  expect_error(GC_theme(chart, textColor = c("red", "blue")), "single character string")
  expect_error(GC_theme(chart, palette = 1:3), "palette")
  expect_error(GC_theme(chart, annotations = list(circle = list())), "Unknown annotation type")
  expect_error(geneviewer_theme("unknown"), "must be one of")
})

test_that("the theme is sent to the browser with its keys there", {
  themed <- GC_theme(
    chart,
    fontFamily = "Arial",
    textColor = "navy",
    lineColor = "grey",
    background = "white",
    palette = "schemeSet2",
    labels = list(fill = "red"),
    links = list(normal_color = "green", color_bar = FALSE),
    scale = list(axis_position = "top"),
    annotations = list(arc = list(style = list(stroke = "blue")))
  )
  js <- compile_chart(themed)$theme

  expect_equal(js$chartOptions$style$backgroundColor, "white")
  expect_equal(js$titleOptions$titleFont, list(fontFamily = "Arial", fill = "navy"))
  expect_equal(js$sequenceOptions$sequenceStyle$stroke, "grey")
  expect_equal(js$scaleOptions$textStyle$fontFamily, "Arial")
  expect_equal(js$tooltipOptions$color, "navy")
  expect_equal(js$geneOptions$colorScheme, "schemeSet2")
  expect_equal(js$legendOptions$legendOptions$colorScheme, "schemeSet2")

  # Options of a component take precedence over the global settings
  expect_equal(js$labelsOptions$fill, "red")
  expect_equal(js$labelsOptions$fontFamily, "Arial")

  # Option names of the GC_ functions are converted
  expect_equal(js$linkOptions$normalColor, "green")
  expect_false(js$linkOptions$colorBar)
  expect_equal(js$scaleOptions$axisPosition, "top")
  expect_equal(js$curveAnnotationOptions$style$stroke, "blue")

  # No global settings: no options
  expect_null(compile_chart(chart)$theme)
  expect_equal(names(theme_js(list(genes = list(stroke = "none")))), "geneOptions")
})

test_that("a palette of colours is sent as an array and unnamed lists replace", {
  js <- theme_js(list(palette = "#ff0000"), simplify = FALSE)
  expect_equal(js$geneOptions$colorScheme, list("#ff0000"))
  expect_equal(js$transcriptOptions$colorScheme, list("#ff0000"))

  # An unnamed list in a component replaces the global palette
  js <- theme_js(list(palette = c("red", "blue"),
                      genes = list(colorScheme = list("green"))), simplify = FALSE)
  expect_equal(js$geneOptions$colorScheme, list("green"))
})

test_that("GC_transcript() keeps the theme, in either order", {
  brca <- subset(BRCA1_splice_variants, ensembl_transcript_id %in% unique(ensembl_transcript_id)[1:2])
  transcripts <- function(chart) {
    GC_transcript(chart, transcript = "ensembl_transcript_id", type = "type")
  }
  chart <- GC_chart(brca, start = "start", end = "end", strand = "strand", height = "200px")

  dark <- compile_chart(GC_theme(chart, "dark"))$theme
  expect_equal(compile_chart(transcripts(GC_theme(chart, "dark")))$theme, dark)
  expect_equal(compile_chart(GC_theme(transcripts(chart), "dark"))$theme, dark)
})

test_that("GC_transcript() keeps the events and toolbar of the chart", {
  brca <- subset(BRCA1_splice_variants, ensembl_transcript_id %in% unique(ensembl_transcript_id)[1:2])
  chart <- GC_chart(brca, start = "start", end = "end", strand = "strand", height = "200px") %>%
    GC_events(select = TRUE, drag = TRUE) %>%
    GC_toolbar(show = "always", position = "bottomleft")

  transcripts <- GC_transcript(chart, transcript = "ensembl_transcript_id", type = "type")
  expect_equal(compile_chart(transcripts)$events, compile_chart(chart)$events)
  expect_equal(compile_chart(transcripts)$toolbar, compile_chart(chart)$toolbar)
})

test_that("the theme is converted when the chart is drawn", {
  themed <- GC_theme(chart, "classic")
  file <- tempfile(fileext = ".html")
  htmlwidgets::saveWidget(themed, file, selfcontained = FALSE)
  html <- paste(readLines(file, warn = FALSE), collapse = "")
  # The page has the theme as set; the library converts it
  expect_match(html, '"theme":\\{"base":"classic"\\}')
  expect_equal(compile_chart(themed)$theme$geneOptions$colorScheme[1], "#5470c6")

  # The chart object keeps the theme as set
  expect_equal(themed$x$spec$theme, list(base = "classic"))
})

test_that("the bw theme fills genes, exons and legend markers with patterns", {
  js <- theme_js(geneviewer_theme("bw"))
  bw_pattern <- list(color = "#000000", background = "#FFFFFF", size = 6, strokeWidth = 1)
  expect_equal(js$geneOptions$pattern, bw_pattern)
  expect_equal(js$transcriptOptions$pattern, bw_pattern)
  expect_equal(js$legendOptions$legendOptions$pattern, bw_pattern)

  # Patterns set with the theme are merged with those of the current theme
  themed <- GC_theme(chart, "bw", pattern = list(patterns = c("dots", "grid"),
                                                 background = "grey90"))
  js <- compile_chart(themed)$theme
  expect_equal(js$geneOptions$pattern$patterns, c("dots", "grid"))
  expect_equal(js$geneOptions$pattern$background, "#E5E5E5")
  expect_equal(js$geneOptions$pattern$color, "#000000")

  # FALSE removes the patterns of the theme
  unpatterned <- compile_chart(GC_theme(themed, pattern = FALSE))$theme
  expect_null(unpatterned$geneOptions$pattern)
  expect_equal(unpatterned$titleOptions$subtitleFont$fill, "#555555")

  expect_error(GC_theme(chart, pattern = "dots"), "arguments of GC_pattern")
  expect_error(GC_theme(chart, pattern = list(type = "dots")), "arguments of GC_pattern")
  expect_error(GC_theme(chart, pattern = list(patterns = "stars")), "Unknown pattern")
})

test_that("textScale is kept in the theme and sent to the browser", {
  themed <- GC_theme(chart, "classic", textScale = 1.5)
  expect_equal(themed$x$spec$theme$textScale, 1.5)
  expect_equal(compile_chart(themed)$theme$textScale, 1.5)
  # Kept when the theme is changed, replaced by a new theme
  expect_equal(compile_chart(GC_theme(themed, textColor = "red"))$theme$textScale, 1.5)
  expect_null(compile_chart(GC_theme(themed, "classic"))$theme$textScale)
  expect_null(theme_js(list(labels = list(fontSize = "10px")))$textScale)

  expect_error(GC_theme(chart, textScale = "large"), "single positive number")
  expect_error(GC_theme(chart, textScale = c(1, 2)), "single positive number")
  expect_error(GC_theme(chart, textScale = 0), "single positive number")
})

test_that("geneviewer palettes are used by name and sent as colours", {
  expect_equal(geneviewer_palette(), c("okabeIto", "classic", "vivid", "dark", "vintage", "infographic", "greys", "blues"))
  expect_equal(geneviewer_theme("vintage")$palette, "vintage")
  vintage <- theme_js(geneviewer_theme("vintage"))
  expect_equal(vintage$geneOptions$cornerRadius, 3)
  expect_equal(vintage$legendOptions$cornerRadius, 3)
  dark <- theme_js(geneviewer_theme("dark"))
  expect_equal(dark$geneOptions$cornerRadius, 3)
  expect_equal(dark$legendOptions$cornerRadius, 3)
  expect_equal(geneviewer_palette("vivid")[1], "#4992ff")
  expect_error(geneviewer_palette("echarts"), "name must be one of")
  expect_equal(geneviewer_theme("classic")$palette, "classic")

  # In a theme, GC_genes() and GC_color(); d3 scheme names are kept
  render <- compile_chart
  expect_equal(render(GC_theme(chart, palette = "blues"))$theme$geneOptions$colorScheme,
               geneviewer_palette("blues"))
  rendered <- render(GC_genes(chart, colorScheme = "vivid"))
  expect_equal(rendered$series[[1]]$genes$colorScheme, geneviewer_palette("vivid"))
  expect_equal(rendered$legend$legendOptions$colorScheme, geneviewer_palette("vivid"))
  expect_equal(render(GC_color(chart, colorScheme = "greys"))$series[[1]]$genes$colorScheme,
               geneviewer_palette("greys"))
  expect_equal(render(GC_genes(chart, colorScheme = "schemeSet2"))$series[[1]]$genes$colorScheme,
               "schemeSet2")
})

test_that("the publication theme follows journal figure guidelines", {
  theme <- geneviewer_theme("publication")
  js <- theme_js(theme)

  expect_equal(theme$fontFamily, "Arial, Helvetica, sans-serif")
  expect_equal(theme$textColor, "black")
  expect_equal(theme$lineColor, "black")
  expect_equal(theme$palette, "okabeIto")
  expect_null(theme$pattern)

  # Journals permit 5--7 pt text other than 8 pt bold panel labels. At the CSS
  # reference resolution, 7 pt is 9.33 px.
  expect_equal(js$titleOptions$titleFont$fontSize, "9.33px")
  expect_equal(js$labelsOptions$fontSize, "9.33px")
  expect_equal(js$labelsOptions$fontStyle, "italic")
  expect_equal(js$scaleOptions$textStyle$fontSize, "9.33px")
  expect_equal(js$legendOptions$legendTextOptions$fontSize, "9.33px")
  expect_equal(js$textAnnotationOptions$style$fontSize, "9.33px")
  expect_equal(js$textMarkerAnnotationOptions$textStyle$fontSize, "9.33px")
  expect_equal(js$curveAnnotationOptions$textStyle$fontSize, "9.33px")

  expect_equal(js$scaleOptions$lineStyle$stroke, "black")
  expect_equal(js$scaleOptions$tickStyle$stroke, "black")
  expect_gt(js$scaleOptions$tickStyle$lineLength, 0)
  expect_equal(js$geneOptions$stroke, "black")
  expect_equal(js$legendOptions$legendOptions$stroke, "black")
  expect_equal(js$tooltipOptions$color, "black")
  expect_equal(js$tooltipOptions$boxShadow, "none")
  expect_equal(js$trackMouseOptions$boxShadow, "none")
})

test_that("the font and text colour of the theme reach the labels of arc annotations", {
  js <- theme_js(list(fontFamily = "Georgia", textColor = "white"))
  expect_equal(js$curveAnnotationOptions$textStyle$fontFamily, "Georgia")
  expect_equal(js$curveAnnotationOptions$textStyle$fill, "white")
  # Options of the arc in the theme take precedence
  themed <- list(textColor = "white", annotations = list(arc = list(textStyle = list(fill = "red"))))
  expect_equal(theme_js(themed)$curveAnnotationOptions$textStyle$fill, "red")
  expect_equal(theme_js(geneviewer_theme("publication"))$curveAnnotationOptions$textStyle$fontFamily,
               "Arial, Helvetica, sans-serif")
})

test_that("the UTRs of transcripts are filled with the background of the theme", {
  expect_equal(theme_js(geneviewer_theme("dark"))$transcriptOptions$styleUTRs$fill, "#333333")
  expect_equal(theme_js(list(background = "black"))$transcriptOptions$styleUTRs$fill, "black")
  expect_null(theme_js(geneviewer_theme("default"))$transcriptOptions$styleUTRs)
  # A fill set for the UTRs in the theme takes precedence
  themed <- list(background = "black", transcript = list(styleUTRs = list(fill = "red")))
  expect_equal(theme_js(themed)$transcriptOptions$styleUTRs$fill, "red")
})

test_that("the publication theme keeps all general text at 7 pt", {
  js <- theme_js(geneviewer_theme("publication"))
  font_sizes <- function(x) {
    if (!is.list(x)) return(NULL)
    sizes <- if (!is.null(x$fontSize)) as.numeric(sub("px$", "", x$fontSize))
    c(sizes, unlist(lapply(x, font_sizes)))
  }
  expect_true(all(font_sizes(js) <= 9.33))
  expect_equal(js$titleOptions$titleFont$fontSize, "9.33px")
  expect_equal(geneviewer_theme("publication")$palette, "okabeIto")
})

test_that("the lines of spread labels take the line colour and their own options of the theme", {
  # The global line colour, with the options of the component on top
  js <- theme_js(list(lineColor = "grey", labels = list(leaderLine = list(strokeWidth = 2))))
  expect_equal(js$labelsOptions$leaderLine, list(stroke = "grey", strokeWidth = 2))
  js <- theme_js(list(lineColor = "grey", labels = list(leader_line = list(stroke = "red"))))
  expect_equal(js$labelsOptions$leaderLine$stroke, "red")
  # FALSE in the theme leaves the lines out
  expect_false(theme_js(list(lineColor = "grey", labels = list(leaderLine = FALSE)))$labelsOptions$leaderLine)

  # Each built-in theme styles them
  line_of <- function(name) theme_js(geneviewer_theme(name))$labelsOptions$leaderLine
  expect_null(line_of("default"))
  expect_equal(line_of("classic"), list(stroke = "#6E7079", strokeWidth = 1))
  expect_equal(line_of("dark"), list(stroke = "#eeeeee", strokeWidth = 1))
  expect_equal(line_of("vintage"), list(stroke = "#c9c2b5", strokeWidth = 1))
  expect_equal(line_of("infographic"), list(stroke = "#6E7079", strokeWidth = 1))
  expect_equal(line_of("publication"), list(stroke = "black", strokeWidth = 0.75))
  expect_equal(line_of("bw"), list(stroke = "#555555", strokeWidth = 1))
})
