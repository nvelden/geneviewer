spec_genes <- data.frame(
  start = c(10, 90, 130, 170, 210),
  end = c(40, 120, 160, 200, 240),
  name = c("Gene 1", "Gene 3", "Gene 4", "Gene 5", "Gene 6"),
  group = c("A", "B", "B", "A", "C"),
  cluster = c(1, 1, 2, 2, 2)
)

spec_chart <- function() {
  GC_chart(spec_genes, cluster = "cluster", group = "group", height = "200px") %>%
    GC_title("Genes", subtitle = "Two clusters") %>%
    GC_labels("name") %>%
    GC_labels(cluster = 2, itemStyle = list(list(index = 0, fill = "red"))) %>%
    GC_genes(marker = "arrow", corner_radius = 2) %>%
    GC_color(customColors = list(A = "red")) %>%
    GC_legend(position = "top", legendTextOptions = list(fontSize = "12px")) %>%
    GC_scale(scale_breaks = TRUE, scale_break_padding = "5bp") %>%
    GC_scaleBar(cluster = 1, scaleBarLineStyle = list(stroke = "red")) %>%
    GC_sequence(sequenceStyle = list(stroke = "grey")) %>%
    GC_clusterTitle(c("One", "Two")) %>%
    GC_clusterLabel() %>%
    GC_clusterFooter("Footer", cluster = "1") %>%
    GC_coordinates() %>%
    GC_cluster(separate_strands = TRUE) %>%
    GC_grid(margin = list(left = 50)) %>%
    GC_annotation(type = "text", text = "Here", x = 100, y = 70, cluster = 1) %>%
    GC_annotation(type = "line", x1 = 140, x2 = 150, cluster = 2) %>%
    GC_tooltip("{name}") %>%
    GC_trackMouse() %>%
    GC_element_style("genes", row_id = 1, fill = "blue") %>%
    GC_element_style("labels", row_id = 2, fill = "green") %>%
    GC_element_style("labels", row_id = 2, reset = TRUE) %>%
    GC_theme("classic", textScale = 1.1) %>%
    GC_events(select = TRUE, legend_click = FALSE) %>%
    GC_toolbar(buttons = c("zoom", "save")) %>%
    GC_zoom(range = c(0.5, 0.1)) %>%
    GC_align("name", "Gene 3")
}

sent_x <- function(widget) {
  htmlwidgets:::createPayload(widget)$x
}

test_that("the widget sends the spec, and only the spec", {
  x <- sent_x(spec_chart())
  expect_named(x, "spec")
  expect_identical(x$spec$version, 1L)
})

test_that("the option of 0.2.x that recorded the spec changes nothing", {
  old <- options(geneviewer.record_spec = FALSE)
  on.exit(options(old))
  off <- sent_x(spec_chart())
  options(geneviewer.record_spec = TRUE)
  on <- sent_x(spec_chart())
  expect_identical(htmlwidgets:::toJSON(on), htmlwidgets:::toJSON(off))
})

test_that("the spec holds the arguments as given, under their spec names", {
  spec <- sent_x(spec_chart())$spec

  expect_identical(spec$version, 1L)
  expect_identical(spec$data, spec_genes)
  expect_identical(spec$fields, list(cluster = "cluster", group = "group"))
  expect_identical(spec$height, "200px")
  expect_null(spec$width)

  # Repeated calls make an array of entries, in call order
  expect_identical(spec$labels, list(
    list(label = "name"),
    list(clusters = list(2), itemStyle = list(list(index = 0, fill = "red")))
  ))
  expect_identical(spec$genes, list(marker = "arrow", cornerRadius = 2))
  expect_identical(spec$legend$textStyle, list(fontSize = "12px"))
  expect_identical(spec$scale, list(autoBreaks = TRUE, breakPadding = "5bp"))
  expect_identical(spec$scaleBar, list(clusters = list(1), lineStyle = list(stroke = "red")))
  expect_identical(spec$sequence, list(lineStyle = list(stroke = "grey")))
  expect_identical(spec$clusterTitle, list(title = c("One", "Two")))
  expect_identical(spec$clusterLabel, structure(list(), names = character(0)))
  expect_identical(spec$cluster, list(separateStrands = TRUE))
  expect_length(spec$annotation, 2)
  expect_identical(spec$annotation[[2]],
                   list(type = "line", clusters = list(2), x1 = 140, x2 = 150))

  # A reset removes the element's earlier entries
  expect_identical(spec$elementStyle,
                   list(list(component = "genes", rowID = 1, fill = "blue")))
  expect_identical(spec$theme, list(base = "classic", textScale = 1.1))
  expect_identical(spec$events, list(select = TRUE, legendClick = FALSE))
  expect_identical(spec$toolbar, list(buttons = list("zoom", "save")))
  expect_identical(spec$zoom, list(range = list(0.5, 0.1)))
  expect_identical(spec$align, list(idColumn = "name", id = "Gene 3"))
})

test_that("calls that change nothing are not recorded", {
  spec <- sent_x(
    GC_chart(spec_genes, group = "group") %>%
      GC_title(show = FALSE) %>%
      GC_labels(show = FALSE)
  )$spec
  expect_null(spec$title)
  expect_null(spec$labels)
})

test_that("a logical for a column argument is recorded as show", {
  spec <- sent_x(GC_chart(spec_genes, group = "group") %>% GC_legend(FALSE))$spec
  expect_identical(spec$legend, list(show = FALSE))
})

test_that("save_button = FALSE is a toolbar without the save button", {
  spec <- sent_x(GC_chart(spec_genes, save_button = FALSE))$spec
  expect_identical(spec$toolbar, list(buttons = list("undo", "redo", "reset", "zoom")))
})

test_that("theme calls without a theme change the one set before", {
  spec <- sent_x(
    GC_chart(spec_genes, group = "group") %>%
      GC_theme("dark", links = list(normal_color = "red", show_links = FALSE)) %>%
      GC_theme(links = list(low_color = "white"), pattern = FALSE,
               legend = list(legendTextOptions = list(fill = "red")))
  )$spec
  expect_identical(spec$theme, list(
    base = "dark",
    links = list(normalColor = "red", show = FALSE, lowColor = "white"),
    pattern = FALSE,
    legend = list(textStyle = list(fill = "red"))
  ))
})

test_that("links from gene matching are recorded as data", {
  genes <- data.frame(
    start = c(1, 50, 100, 1, 50),
    end = c(40, 90, 140, 40, 90),
    name = c("a", "b", "c", "a", "b"),
    cluster = c("x", "x", "x", "y", "y")
  )
  chart <- GC_chart(genes, cluster = "cluster") %>% GC_links("name", measure = "none")
  spec <- sent_x(chart)$spec
  expect_identical(spec$links$group, "name")
  expect_identical(spec$links$measure, "none")
  expect_identical(nrow(spec$links$data), 2L)
  expect_true(all(c("name1", "start1", "cluster1", "start2", "cluster2") %in%
                    names(spec$links$data)))
})

test_that("transcripts are recorded with their introns", {
  exons <- data.frame(
    start = c(1, 50), end = c(20, 80),
    type = "exon", transcript = "t1"
  )
  spec <- sent_x(GC_chart(exons) %>% GC_transcript("transcript", type = "type"))$spec
  expect_identical(spec$transcript, list(transcript = "transcript", type = "type"))
  expect_true("intron" %in% spec$data$type)
  expect_null(spec$grid)
})

test_that("normalised positions are recorded in the data", {
  chart <- GC_chart(spec_genes, cluster = "cluster", group = "group") %>%
    GC_normalize(gap = 0.1)
  x <- sent_x(chart)
  expect_null(x$spec$normalize)
  # The library draws the positions of the spec
  model <- compile_chart(chart)
  expect_equal(unlist(lapply(model$series, function(s) s$data$start), use.names = FALSE),
               x$spec$data$start)
})

test_that("the spec validates against the schema of spec v1", {
  skip_unless(requireNamespace("jsonvalidate", quietly = TRUE), "jsonvalidate cannot be loaded")
  schema <- Sys.getenv("GENEVIEWER_SPEC_SCHEMA")
  skip_unless(nzchar(schema) && file.exists(schema),
              "GENEVIEWER_SPEC_SCHEMA is not set to spec-v1.schema.json")
  json <- htmlwidgets:::toJSON(sent_x(spec_chart())$spec)
  expect_true(jsonvalidate::json_validate(json, schema, engine = "ajv", error = TRUE))
})

test_that("options passed through ... keep their names", {
  spec <- sent_x(GC_chart(spec_genes, group = "group") %>%
                   GC_genes(marker_size = "small", prevent_overlap = FALSE))$spec
  expect_identical(spec$genes, list(markerSize = "small", prevent_overlap = FALSE))
})

test_that("NULL is kept where it replaces a default", {
  spec <- sent_x(GC_chart(spec_genes, group = "group") %>%
                   GC_pattern(background = NULL, size = 4))$spec
  expect_identical(spec$pattern, list(background = NULL, size = 4))
  expect_match(as.character(htmlwidgets:::toJSON(spec$pattern)), '"background":null')
})

test_that("options passed through ... that spec v1 renames get the spec name", {
  spec <- sent_x(GC_chart(spec_genes, group = "group") %>%
                   GC_scaleBar(scaleBarUnit = "kb", title = "1 kb") %>%
                   GC_labels("name", labelAdjustmentOptions = list(offsetY = 2)))$spec
  expect_identical(spec$scaleBar, list(unit = "kb", title = "1 kb"))
  expect_identical(spec$labels, list(label = "name", labelAdjustment = list(offsetY = 2)))
})

test_that("sequence columns are sent only when an option names them", {
  genes <- data.frame(start = c(10, 50), end = c(40, 80), name = c("a", "b"),
                      translation = c("MKV", "MLL"), sequence = c("ATG", "ATG"))
  sent <- function(chart) htmlwidgets:::createPayload(chart)$x$spec$data
  chart <- GC_chart(genes, height = "100px")
  expect_named(sent(chart), c("start", "end", "name"))
  # The chart keeps its data as given
  expect_true(all(c("translation", "sequence") %in% names(chart$x$spec$data)))
  expect_named(sent(GC_tooltip(chart, formatter = "{name}: {translation}")),
               c("start", "end", "name", "translation"))
})

test_that("chart_fields() gives the columns and clusters of the prepared data", {
  same_as_prepared <- function(chart) {
    state <- chart_state(chart)
    expect_identical(state$columns, names(state$data))
    expect_identical(state$clusters, state_clusters(state$data, state$cluster))
  }
  genes <- data.frame(from = c(10, 50, NA, 90), to = c(40, 80, 30, 120),
                      contig = factor(c("b", "b", "gone", "a")), kind = c("x", "y", "x", "x"),
                      dir = c(1, -1, 1, -1))

  same_as_prepared(suppressWarnings(GC_chart(genes, start = "from", end = "to")))
  same_as_prepared(suppressWarnings(GC_chart(genes, "from", "to", "contig", "kind", "dir")))
  same_as_prepared(GC_chart(data.frame(genes, cluster = c(2, 2, 3, 1), start = 1, end = 2),
                            cluster = "cluster"))
  # An NA is a cluster; a cluster with only rows left out is not
  with_na <- suppressWarnings(GC_chart(transform(genes, contig = c(NA, "b", "gone", "a")),
                                       "from", "to", "contig"))
  same_as_prepared(with_na)
  expect_identical(chart_fields(with_na)$clusters, c(NA, "b", "a"))
  # No rows left: as GC_chart() allows it, without columns or clusters
  expect_null(chart_fields(suppressWarnings(GC_chart(genes[3, ], "from", "to", "contig")))$columns)
  same_as_prepared(suppressWarnings(GC_chart(genes[3, ], "from", "to", "contig")))
  same_as_prepared(suppressWarnings(GC_chart(genes[3, ], "from", "to")))

  # After GC_transcript(), the transcripts are the clusters
  exons <- data.frame(start = c(1, 50, 200), end = c(20, 80, 260), type = "exon",
                      transcript = c("t2", "t2", "t1"), strand = 1)
  transcripts <- GC_chart(exons, group = "type") %>% GC_transcript(transcript = "transcript", type = "type")
  same_as_prepared(transcripts)
  expect_identical(chart_fields(transcripts)$clusters, c("t2", "t1"))
})

test_that("theme settings keep or remove a NULL, and replace unnamed lists", {
  old <- list(a = list(b = 1, c = 2), d = 3, e = list("x", "y"))
  new <- list(a = list(b = NULL), d = NULL, e = list("z"))
  expect_identical(merge_theme_settings(old, new),
                   list(a = list(c = 2), e = list("z")))
  expect_identical(merge_spec_theme(old, new),
                   list(a = list(b = NULL, c = 2), d = NULL, e = list("z")))
})
