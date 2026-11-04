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

# Records the spec (or not) until the test ends
local_record_spec <- function(value, env = parent.frame()) {
  old <- options(geneviewer.record_spec = value)
  do.call(on.exit, list(substitute(options(old)), add = TRUE, after = FALSE), envir = env)
}

test_that("without the option the chart has no spec", {
  local_record_spec(NULL)
  expect_null(sent_x(spec_chart())$spec)
})

test_that("recording the spec does not change the chart", {
  local_record_spec(NULL)
  off <- sent_x(spec_chart())
  local_record_spec(TRUE)
  on <- sent_x(spec_chart())
  expect_false(is.null(on$spec))
  on$spec <- NULL
  expect_identical(htmlwidgets:::toJSON(on), htmlwidgets:::toJSON(off))
})

test_that("the spec holds the arguments as given, under their spec names", {
  local_record_spec(TRUE)
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
  local_record_spec(TRUE)
  spec <- sent_x(
    GC_chart(spec_genes, group = "group") %>%
      GC_title(show = FALSE) %>%
      GC_labels(show = FALSE)
  )$spec
  expect_null(spec$title)
  expect_null(spec$labels)
})

test_that("a logical for a column argument is recorded as show", {
  local_record_spec(TRUE)
  spec <- sent_x(GC_chart(spec_genes, group = "group") %>% GC_legend(FALSE))$spec
  expect_identical(spec$legend, list(show = FALSE))
})

test_that("save_button = FALSE is a toolbar without the save button", {
  local_record_spec(TRUE)
  spec <- sent_x(GC_chart(spec_genes, save_button = FALSE))$spec
  expect_identical(spec$toolbar, list(buttons = list("undo", "redo", "reset", "zoom")))
})

test_that("theme calls without a theme change the one set before", {
  local_record_spec(TRUE)
  spec <- sent_x(
    GC_chart(spec_genes, group = "group") %>%
      GC_theme("dark", links = list(normal_color = "red")) %>%
      GC_theme(links = list(low_color = "white"), pattern = FALSE)
  )$spec
  expect_identical(spec$theme, list(
    base = "dark",
    links = list(normalColor = "red", lowColor = "white"),
    pattern = FALSE
  ))
})

test_that("links from gene matching are recorded as data", {
  local_record_spec(TRUE)
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
  local_record_spec(TRUE)
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
  local_record_spec(TRUE)
  chart <- GC_chart(spec_genes, cluster = "cluster", group = "group") %>%
    GC_normalize(gap = 0.1)
  x <- sent_x(chart)
  expect_identical(x$spec$data$start, chart$x$data$start)
  expect_null(x$spec$normalize)
})

test_that("the spec validates against the schema of spec v1", {
  skip_if_not_installed("jsonvalidate")
  schema <- Sys.getenv("GENEVIEWER_SPEC_SCHEMA")
  skip_if(!nzchar(schema) || !file.exists(schema),
          "GENEVIEWER_SPEC_SCHEMA is not set to spec-v1.schema.json")
  local_record_spec(TRUE)
  json <- htmlwidgets:::toJSON(sent_x(spec_chart())$spec)
  expect_true(jsonvalidate::json_validate(json, schema, engine = "ajv", error = TRUE))
})
