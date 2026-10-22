chart <- GC_chart(ophA_clusters, cluster = "cluster", group = "class", height = "200px")

test_that("GC_theme() starts from a built-in theme and changes the current theme", {
  themed <- GC_theme(chart, "dark")
  expect_equal(themed$x$theme, geneviewer_theme("dark"))

  # Without a theme, the current theme is changed
  themed <- GC_theme(themed, textColor = "white", labels = list(fontSize = "9px"))
  expect_equal(themed$x$theme$textColor, "white")
  expect_equal(themed$x$theme$background, "#1e1e1e")
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
  expect_match(html, '"geneOptions":\\{"colorScheme":\\["#E69F00"')

  # The chart object keeps the theme as set
  expect_equal(themed$x$theme$palette, geneviewer_theme("classic")$palette)
})
