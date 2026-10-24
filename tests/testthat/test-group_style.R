# groupStyle and style_by of GC_genes(), GC_labels(), GC_legend(),
# GC_transcript() and GC_coordinates()

group_genes <- data.frame(
  start = c(10, 50, 90, 130),
  end = c(40, 80, 120, 160),
  name = c("a1", "b1", "a2", "c1"),
  class = c("A", "B", "A", "C"),
  family = c("x", "y", "y", "x"),
  cluster = 1
)

transcripts <- data.frame(
  transcript = c("tx1", "tx1", "tx1", "tx1", "tx2", "tx2", "tx2"),
  type = c("5_utr", "exon", "exon", "3_utr", "5_utr", "exon", "3_utr"),
  start = c(1, 101, 201, 301, 1, 101, 301),
  end = c(50, 150, 250, 350, 50, 150, 350),
  strand = "forward"
)

# Renders a chart in headless Chrome and returns the value of a JavaScript
# expression run on the page
in_chrome <- function(chart, js, env = parent.frame()) {
  file <- tempfile(fileext = ".html")
  htmlwidgets::saveWidget(chart, file, selfcontained = FALSE)
  session <- chrome_page(file, env = env)
  session$Runtime$evaluate(js, returnByValue = TRUE)$result$value
}

# The computed value of a style of each element matching a selector
computed <- function(selector, property) {
  sprintf("[...document.querySelectorAll('%s')].map(e => getComputedStyle(e)['%s'])",
          selector, property)
}

# R side ----------------------------------------------------------------------

test_that("GC_legend stores and checks groupStyle", {
  chart <- GC_chart(group_genes, group = "class") %>%
    GC_legend(groupStyle = list(A = list(fontStyle = "italic",
                                         marker = list(stroke = "red"))))
  expect_equal(chart$x$legend$groupStyle$A$fontStyle, "italic")
  expect_equal(chart$x$legend$groupStyle$A$marker$stroke, "red")
  expect_null((GC_chart(group_genes, group = "class") %>% GC_legend())$x$legend$groupStyle)
  expect_error(GC_chart(group_genes, group = "class") %>% GC_legend(groupStyle = list("bold")),
               "named list")
  expect_error(GC_chart(group_genes, group = "class") %>% GC_legend(groupStyle = list(A = "bold")),
               "named list")
})

test_that("GC_transcript stores and checks its group styles", {
  chart <- GC_chart(transcripts) %>%
    GC_transcript(transcript = "transcript", type = "type", strand = "strand",
                  style_by = "transcript",
                  groupStyleExons = list(tx1 = list(fill = "red")),
                  groupStyleIntrons = list(tx1 = list(stroke = "blue")),
                  groupStyleUTRs = list(tx2 = list(fill = "green")))
  options <- chart$x$series[["tx1"]]$transcript
  expect_equal(options$groupStyleExons, list(tx1 = list(fill = "red")))
  expect_equal(options$groupStyleIntrons, list(tx1 = list(stroke = "blue")))
  expect_equal(options$groupStyleUTRs, list(tx2 = list(fill = "green")))
  expect_equal(options$styleBy, "transcript")
  # left out when not given
  plain <- GC_chart(transcripts) %>% GC_transcript(transcript = "transcript", type = "type")
  expect_null(plain$x$series[["tx1"]]$transcript$groupStyleExons)
  expect_null(plain$x$series[["tx1"]]$transcript$styleBy)
  expect_error(GC_chart(transcripts) %>%
                 GC_transcript(transcript = "transcript", type = "type",
                               groupStyleExons = list(tx1 = "red")), "named list")
  expect_error(GC_chart(transcripts) %>%
                 GC_transcript(transcript = "transcript", type = "type",
                               groupStyleUTRs = list(list(fill = "red"))), "named list")
  expect_error(GC_chart(transcripts) %>%
                 GC_transcript(transcript = "transcript", type = "type", style_by = "nope"),
               "single column name")
})

test_that("GC_coordinates stores and checks groupStyle and style_by", {
  chart <- GC_chart(group_genes, group = "class") %>%
    GC_coordinates(groupStyle = list(B = list(show = FALSE)))
  options <- chart$x$series[[1]]$coordinates
  expect_equal(options$groupStyle, list(B = list(show = FALSE)))
  # the group of GC_chart(), unless style_by is given
  expect_equal(options$styleBy, "class")
  by_family <- GC_chart(group_genes, group = "class") %>%
    GC_coordinates(style_by = "family", groupStyle = list(x = list(fill = "red")))
  expect_equal(by_family$x$series[[1]]$coordinates$styleBy, "family")
  # nothing is stored without groupStyle
  plain <- GC_chart(group_genes, group = "class") %>% GC_coordinates()
  expect_false(any(c("groupStyle", "styleBy") %in% names(plain$x$series[[1]]$coordinates)))
  # a groupStyle needs a column to match against
  expect_error(GC_chart(group_genes[, c("start", "end")]) %>%
                 GC_coordinates(groupStyle = list(A = list(show = FALSE))), "style_by")
  expect_error(GC_chart(group_genes, group = "class") %>%
                 GC_coordinates(groupStyle = list(A = TRUE)), "named list")
  expect_error(GC_chart(group_genes, group = "class") %>%
                 GC_coordinates(style_by = "nope"), "single column name")
})

# In the browser ---------------------------------------------------------------

test_that("GC_genes draws the groups of groupStyle, matched by style_by", {
  chart <- GC_chart(group_genes, group = "class", height = "150px") %>%
    GC_genes(style_by = "family",
             groupStyle = list(y = list(fill = "rgb(255, 0, 0)")),
             itemStyle = list(list(index = 2, fill = "rgb(0, 0, 255)")))
  fills <- unlist(in_chrome(chart, computed(".gene", "fill")))
  # genes in start order: a1 (x), b1 (y), a2 (y, own itemStyle), c1 (x)
  expect_equal(fills[2], "rgb(255, 0, 0)")
  expect_equal(fills[3], "rgb(0, 0, 255)")
  expect_false(fills[1] %in% c("rgb(255, 0, 0)", "rgb(0, 0, 255)"))
  expect_false(fills[4] %in% c("rgb(255, 0, 0)", "rgb(0, 0, 255)"))
})

test_that("GC_labels styles labels by their text with style_by", {
  chart <- GC_chart(group_genes, group = "class", height = "150px") %>%
    GC_labels(label = "name", style_by = "name",
              groupStyle = list(b1 = list(fill = "rgb(255, 0, 0)",
                                          fontWeight = "bold")))
  labels <- in_chrome(chart, "[...document.querySelectorAll('text.label')].map(e =>
      [e.textContent, getComputedStyle(e).fill, getComputedStyle(e).fontWeight])")
  styled <- Filter(function(l) l[[1]] == "b1", labels)[[1]]
  others <- Filter(function(l) l[[1]] != "b1", labels)
  expect_equal(styled[[2]], "rgb(255, 0, 0)")
  expect_equal(styled[[3]], "700")
  expect_true(all(vapply(others, function(l) l[[2]] != "rgb(255, 0, 0)", logical(1))))
})

test_that("GC_legend styles the text and marker of a group", {
  chart <- GC_chart(group_genes, group = "class", width = "600px", height = "150px") %>%
    GC_legend(groupStyle = list(
      A = list(fontStyle = "italic"),
      B = list(fill = "rgb(255, 0, 0)", marker = list(stroke = "rgb(0, 128, 0)"))
    ))
  legend <- in_chrome(chart, "[...document.querySelectorAll('.legend')].map(g => {
      const text = g.querySelector('text'), rect = g.querySelector('rect');
      return [text.textContent, getComputedStyle(text).fontStyle,
              getComputedStyle(text).fill, getComputedStyle(rect).stroke];
    })")
  entry <- function(group) Filter(function(l) l[[1]] == group, legend)[[1]]
  expect_equal(entry("A")[[2]], "italic")
  expect_equal(entry("B")[[3]], "rgb(255, 0, 0)")
  expect_equal(entry("B")[[4]], "rgb(0, 128, 0)")
  # other groups keep the defaults
  expect_equal(entry("C")[[2]], "normal")
  expect_false(entry("C")[[3]] == "rgb(255, 0, 0)")
})

test_that("a larger legend text from groupStyle does not overlap the next entry", {
  chart <- GC_chart(group_genes, group = "class", width = "600px", height = "150px") %>%
    GC_legend(groupStyle = list(A = list(fontSize = "24px", fontWeight = "bold")))
  boxes <- in_chrome(chart, "[...document.querySelectorAll('.legend')].map(g => {
      const r = g.getBoundingClientRect(); return [r.left, r.right, r.top];
    })")
  # entries on the same row, in order, each starting after the previous ends
  rows <- split(boxes, vapply(boxes, function(b) round(b[[3]]), numeric(1)))
  for (row in rows) {
    row <- row[order(vapply(row, `[[`, numeric(1), 1))]
    for (k in seq_along(row)[-1]) {
      expect_gte(row[[k]][[1]], row[[k - 1]][[2]] - 0.5)
    }
  }
})

test_that("GC_transcript styles exons, introns and UTRs by group", {
  chart <- GC_chart(transcripts, height = "200px") %>%
    GC_transcript(transcript = "transcript", type = "type", strand = "strand",
                  style_by = "transcript",
                  groupStyleExons = list(tx1 = list(fill = "rgb(255, 0, 0)")),
                  groupStyleIntrons = list(tx1 = list(stroke = "rgb(0, 0, 255)")),
                  groupStyleUTRs = list(tx2 = list(fill = "rgb(0, 128, 0)")),
                  itemStyleExons = list(list(index = 1, fill = "rgb(0, 0, 0)")))
  parts <- in_chrome(chart, "[...document.querySelectorAll('.exon, .intron, .utr')].map(e =>
      [e.getAttribute('class'), e.closest('svg').parentNode.id, e.getAttribute('id'),
       getComputedStyle(e).fill, getComputedStyle(e).stroke])")
  part <- function(cls, id) Filter(function(p) p[[1]] == cls && p[[3]] == id, parts)[[1]]
  # tx1: two exons (the second has its own itemStyle), one intron and two UTRs
  expect_equal(part("exon", "tx1-exon-0")[[4]], "rgb(255, 0, 0)")
  expect_equal(part("exon", "tx1-exon-1")[[4]], "rgb(0, 0, 0)")
  expect_equal(part("intron", "tx1-intron-0")[[5]], "rgb(0, 0, 255)")
  expect_false(part("utr", "tx1-utr-0")[[4]] == "rgb(0, 128, 0)")
  # tx2: its exon is not styled, its UTRs are
  expect_false(part("exon", "tx2-exon-0")[[4]] %in% c("rgb(255, 0, 0)", "rgb(0, 0, 0)"))
  expect_equal(part("utr", "tx2-utr-0")[[4]], "rgb(0, 128, 0)")
  expect_equal(part("utr", "tx2-utr-1")[[4]], "rgb(0, 128, 0)")
})

test_that("GC_transcript group styles can change the marker of a group", {
  chart <- GC_chart(transcripts, height = "200px") %>%
    GC_transcript(transcript = "transcript", type = "type", strand = "strand",
                  style_by = "transcript",
                  groupStyleExons = list(tx2 = list(markerHeight = 40)))
  heights <- in_chrome(chart, "Object.fromEntries([...document.querySelectorAll('.exon')].map(e =>
      [e.getAttribute('id'), e.getBBox().height]))")
  expect_gt(heights[["tx2-exon-0"]], heights[["tx1-exon-0"]])
})

coordinate_js <- "[...document.querySelectorAll('.tick')].filter(t => t.querySelector('.coordinate')).map(t =>
    [t.querySelector('text').textContent, getComputedStyle(t.querySelector('text')).fill,
     getComputedStyle(t.querySelector('line')).stroke])"

test_that("GC_coordinates leaves out and styles the coordinates of a group", {
  chart <- GC_chart(group_genes, group = "class", height = "200px") %>%
    GC_coordinates(groupStyle = list(
      B = list(show = FALSE),
      C = list(fill = "rgb(255, 0, 0)", tick = list(stroke = "rgb(0, 0, 255)"))
    ))
  ticks <- in_chrome(chart, coordinate_js)
  values <- vapply(ticks, `[[`, character(1), 1)
  # B (b1, 50 - 80) is left out, the others are there
  expect_false(any(c("50", "80") %in% values))
  expect_true(all(c("10", "40", "90", "120", "130", "160") %in% values))
  tick <- function(value) Filter(function(t) t[[1]] == value, ticks)[[1]]
  # C (c1, 130 - 160) has red text and a blue tick, A keeps the defaults
  expect_equal(tick("130")[[2]], "rgb(255, 0, 0)")
  expect_equal(tick("160")[[3]], "rgb(0, 0, 255)")
  expect_false(tick("10")[[2]] == "rgb(255, 0, 0)")
  expect_false(tick("10")[[3]] == "rgb(0, 0, 255)")
})

test_that("a coordinate a hidden group shares with a shown one is kept", {
  # b1 ends where a2 starts
  genes <- data.frame(start = c(10, 50, 80), end = c(40, 80, 120),
                      class = c("A", "B", "A"))
  chart <- GC_chart(genes, group = "class", height = "200px") %>%
    GC_coordinates(groupStyle = list(B = list(show = FALSE)))
  values <- vapply(in_chrome(chart, coordinate_js), `[[`, character(1), 1)
  expect_true("80" %in% values)
  expect_false("50" %in% values)
})

test_that("GC_coordinates matches groupStyle against style_by", {
  chart <- GC_chart(group_genes, group = "class", height = "200px") %>%
    GC_coordinates(style_by = "family", groupStyle = list(y = list(show = FALSE)))
  values <- vapply(in_chrome(chart, coordinate_js), `[[`, character(1), 1)
  # family y: b1 (50 - 80) and a2 (90 - 120)
  expect_false(any(c("50", "80", "90", "120") %in% values))
  expect_true(all(c("10", "40", "130", "160") %in% values))
})

test_that("charts without group styles draw as before", {
  chart <- GC_chart(group_genes, group = "class", height = "200px") %>%
    GC_labels("name") %>%
    GC_coordinates()
  values <- vapply(in_chrome(chart, coordinate_js), `[[`, character(1), 1)
  expect_setequal(values, c("10", "40", "50", "80", "90", "120", "130", "160"))
})
