chart <- GC_chart(ophA_clusters, cluster = "cluster", group = "class", height = "200px")

test_that("GC_theme() starts from a built-in theme and changes the current theme", {
  themed <- GC_theme(chart, "dark")
  expect_equal(themed$x$theme, geneviewer_theme("dark"))

  # Without a theme, the current theme is changed
  themed <- GC_theme(themed, textColor = "white", labels = list(fontSize = "9px"))
  expect_equal(themed$x$theme$textColor, "white")
  expect_equal(themed$x$theme$background, "#333333")
  expect_equal(themed$x$theme$labels$fontSize, "9px")

  # Nested options are merged, NULL removes a component
  themed <- GC_theme(themed, links = list(linkStyle = list(stroke = "red")))
  expect_equal(themed$x$theme$links$linkStyle, list(fillOpacity = 0.6, stroke = "red"))
  themed <- GC_theme(themed, links = NULL)
  expect_null(themed$x$theme$links)

  # A new theme replaces the current one
  expect_equal(GC_theme(themed, "default")$x$theme, list())
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
  js <- theme_to_js(themed$x$theme)

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
  expect_null(theme_to_js(list()))
  expect_equal(names(theme_to_js(list(genes = list(stroke = "none")))), "geneOptions")
})

test_that("a palette of colours is sent as an array and unnamed lists replace", {
  js <- theme_to_js(list(palette = "#ff0000"))
  expect_equal(js$geneOptions$colorScheme, list("#ff0000"))
  expect_equal(js$transcriptOptions$colorScheme, list("#ff0000"))

  # An unnamed list in a component replaces the global palette
  js <- theme_to_js(list(palette = c("red", "blue"),
                         genes = list(colorScheme = list("green"))))
  expect_equal(js$geneOptions$colorScheme, list("green"))
})

test_that("GC_transcript() keeps the theme, in either order", {
  brca <- subset(BRCA1_splice_variants, ensembl_transcript_id %in% unique(ensembl_transcript_id)[1:2])
  transcripts <- function(chart) {
    GC_transcript(chart, transcript = "ensembl_transcript_id", type = "type")
  }
  chart <- GC_chart(brca, start = "start", end = "end", strand = "strand", height = "200px")

  expect_equal(transcripts(GC_theme(chart, "dark"))$x$theme, geneviewer_theme("dark"))
  expect_equal(GC_theme(transcripts(chart), "dark")$x$theme, geneviewer_theme("dark"))
})

test_that("the theme is converted when the chart is drawn", {
  themed <- GC_theme(chart, "classic")
  file <- tempfile(fileext = ".html")
  htmlwidgets::saveWidget(themed, file, selfcontained = FALSE)
  html <- paste(readLines(file, warn = FALSE), collapse = "")
  expect_match(html, '"theme":\\{"chartOptions"|"theme":\\{"titleOptions"')
  expect_match(html, '"geneOptions":\\{"colorScheme":\\["#5470c6"')

  # The chart object keeps the theme as set
  expect_equal(themed$x$theme$palette, geneviewer_theme("classic")$palette)
})

test_that("the bw theme fills genes, exons and legend markers with patterns", {
  js <- theme_to_js(geneviewer_theme("bw"))
  bw_pattern <- list(color = "#000000", background = "#FFFFFF", size = 6, strokeWidth = 1)
  expect_equal(js$geneOptions$pattern, bw_pattern)
  expect_equal(js$transcriptOptions$pattern, bw_pattern)
  expect_equal(js$legendOptions$legendOptions$pattern, bw_pattern)

  # Patterns set with the theme are merged with those of the current theme
  themed <- GC_theme(chart, "bw", pattern = list(patterns = c("dots", "grid"),
                                                 background = "grey90"))
  js <- theme_to_js(themed$x$theme)
  expect_equal(js$geneOptions$pattern$patterns, list("dots", "grid"))
  expect_equal(js$geneOptions$pattern$background, "#E5E5E5")
  expect_equal(js$geneOptions$pattern$color, "#000000")

  # FALSE removes the patterns of the theme
  expect_null(GC_theme(themed, pattern = FALSE)$x$theme$pattern)
  expect_equal(GC_theme(themed, pattern = FALSE)$x$theme$textColor, "#555555")

  expect_error(GC_theme(chart, pattern = "dots"), "arguments of GC_pattern")
  expect_error(GC_theme(chart, pattern = list(type = "dots")), "arguments of GC_pattern")
  expect_error(GC_theme(chart, pattern = list(patterns = "stars")), "Unknown pattern")
})

test_that("textScale is kept in the theme and sent to the browser", {
  themed <- GC_theme(chart, "minimal", textScale = 1.5)
  expect_equal(themed$x$theme$textScale, 1.5)
  expect_equal(theme_to_js(themed$x$theme)$textScale, 1.5)
  # Kept when the theme is changed, replaced by a new theme
  expect_equal(GC_theme(themed, textColor = "red")$x$theme$textScale, 1.5)
  expect_null(GC_theme(themed, "classic")$x$theme$textScale)
  expect_null(theme_to_js(list(labels = list(fontSize = "10px")))$textScale)

  expect_error(GC_theme(chart, textScale = "large"), "single positive number")
  expect_error(GC_theme(chart, textScale = c(1, 2)), "single positive number")
  expect_error(GC_theme(chart, textScale = 0), "single positive number")
})

test_that("geneviewer palettes are used by name and sent as colours", {
  expect_equal(geneviewer_palette(), c("okabeIto", "classic", "vivid", "dark", "vintage", "infographic", "greys", "blues"))
  expect_equal(geneviewer_theme("vintage")$palette, "vintage")
  vintage <- theme_to_js(geneviewer_theme("vintage"))
  expect_equal(vintage$geneOptions$cornerRadius, 3)
  expect_equal(vintage$legendOptions$cornerRadius, 3)
  dark <- theme_to_js(geneviewer_theme("dark"))
  expect_equal(dark$geneOptions$cornerRadius, 3)
  expect_equal(dark$legendOptions$cornerRadius, 3)
  expect_equal(geneviewer_palette("vivid")[1], "#4992ff")
  expect_error(geneviewer_palette("echarts"), "name must be one of")
  expect_equal(geneviewer_theme("classic")$palette, "classic")

  # In a theme, GC_genes() and GC_color(); d3 scheme names are kept
  render <- function(x) render_theme(x)$x
  expect_equal(render(GC_theme(chart, palette = "blues"))$theme$geneOptions$colorScheme,
               as.list(geneviewer_palette("blues")))
  rendered <- render(GC_genes(chart, colorScheme = "vivid"))
  expect_equal(rendered$series[[1]]$genes$colorScheme, as.list(geneviewer_palette("vivid")))
  expect_equal(rendered$legend$legendOptions$colorScheme, as.list(geneviewer_palette("vivid")))
  expect_equal(render(GC_color(chart, colorScheme = "greys"))$series[[1]]$genes$colorScheme,
               as.list(geneviewer_palette("greys")))
  expect_equal(render(GC_genes(chart, colorScheme = "schemeSet2"))$series[[1]]$genes$colorScheme,
               "schemeSet2")
})

test_that("the UTRs of transcripts are filled with the background of the theme", {
  expect_equal(theme_to_js(geneviewer_theme("dark"))$transcriptOptions$styleUTRs$fill, "#333333")
  expect_equal(theme_to_js(list(background = "black"))$transcriptOptions$styleUTRs$fill, "black")
  expect_null(theme_to_js(geneviewer_theme("minimal"))$transcriptOptions$styleUTRs)
  # A fill set for the UTRs in the theme takes precedence
  themed <- list(background = "black", transcript = list(styleUTRs = list(fill = "red")))
  expect_equal(theme_to_js(themed)$transcriptOptions$styleUTRs$fill, "red")
})
