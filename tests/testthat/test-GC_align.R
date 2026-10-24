# Two clusters of different lengths, with gene "x" at a different place in each
align_genes <- data.frame(
  start = c(100, 300, 600, 1000, 1500, 1800),
  end = c(200, 400, 900, 1200, 1700, 2600),
  name = c("a", "x", "b", "a", "x", "b"),
  cluster = c(1, 1, 1, 2, 2, 2)
)

scale_range <- function(chart) {
  t(sapply(c("1", "2"), function(cl) {
    unlist(chart$x$series[[cl]]$scale[c("start", "end")])
  }))
}

test_that("GC_align shifts the clusters and keeps one scale", {
  chart <- GC_chart(align_genes, cluster = "cluster", group = "name") %>%
    GC_align("name", "x", align = "left")
  range <- scale_range(chart)

  # every cluster spans the same number of base pairs
  expect_equal(range[1, "end"] - range[1, "start"], range[2, "end"] - range[2, "start"])
  # and the gene starts at the same distance from the start of each
  expect_equal(unname(range[, "start"]), c(300, 1500) - 500)
  # longest stretch left of x (500) plus right of it (1100), both in cluster 2
  expect_equal(unname(range[1, "end"] - range[1, "start"]), 1600)
  # the whole of each cluster is in its range
  expect_true(all(range[, "start"] <= c(100, 1000)))
  expect_true(all(range[, "end"] >= c(900, 2600)))
})

test_that("GC_align aligns on the right or centre of the gene", {
  right <- scale_range(GC_chart(align_genes, cluster = "cluster", group = "name") %>%
                         GC_align("name", "x", align = "right"))
  expect_equal(unname(c(400, 1700) - right[, "start"]), c(700, 700))

  center <- scale_range(GC_chart(align_genes, cluster = "cluster", group = "name") %>%
                          GC_align("name", "x", align = "center"))
  expect_equal(unname(c(350, 1600) - center[, "start"]), c(600, 600))
  expect_equal(unname(center[, "end"] - center[, "start"]), c(1600, 1600))
})

test_that("GC_align swaps the sides of a reversed cluster", {
  chart <- GC_chart(align_genes, cluster = "cluster", group = "name") %>%
    GC_scale(cluster = 2, reverse = TRUE) %>%
    GC_align("name", "x", align = "left")
  range <- scale_range(chart)

  # cluster 2 is drawn from its end: the left of x is at 1700, with 900 bp of
  # cluster 2 to its left on screen and 700 bp (cluster 2) to its right
  expect_equal(unname(range[, "end"] - range[, "start"]), c(1600, 1600))
  expect_equal(unname(range[1, "start"]), 300 - 900)
  expect_equal(unname(range[2, "end"]), 1700 + 900)
})

test_that("GC_align is kept by GC_scale, also with a range axis", {
  after <- GC_chart(align_genes, cluster = "cluster", group = "name") %>%
    GC_scale(axis_type = "range") %>%
    GC_align("name", "x")
  before <- GC_chart(align_genes, cluster = "cluster", group = "name") %>%
    GC_align("name", "x") %>%
    GC_scale(axis_type = "range")

  expect_equal(scale_range(before), scale_range(after))
  # the genes now start at 1, so x starts at 201 and 501
  expect_equal(unname(scale_range(after)[, "start"]), c(201, 501) - 500)
  expect_equal(before$x$series[["2"]]$scale$axisType, "range")
})

test_that("GC_scale takes breaks per cluster, in the positions of the data", {
  chart <- GC_chart(align_genes, cluster = "cluster", group = "name") %>%
    GC_scale(axis_type = "range",
             breaks = data.frame(cluster = 2, start = 1250, end = 1450))
  # cluster 2 starts at 1000, so on the range axis the break is at 251 - 451
  expect_equal(chart$x$series[["2"]]$scale$breaks, list(list(start = 251, end = 451)))
  expect_equal(chart$x$series[["1"]]$scale$breaks, list())
  # without a cluster column the breaks are for all clusters, as a list is
  all <- GC_chart(align_genes, cluster = "cluster", group = "name") %>%
    GC_scale(breaks = data.frame(start = 1250, end = 1450))
  expect_equal(all$x$series[["1"]]$scale$breaks, list(list(start = 1250, end = 1450)))
  expect_error(GC_chart(align_genes, cluster = "cluster") %>%
                 GC_scale(breaks = data.frame(from = 1)), "start and an end")
})

test_that("GC_align leaves the scale breaks out of the shared range", {
  breaks <- data.frame(cluster = 2, start = 1250, end = 1450)
  ranges <- function(chart) scale_range(chart)
  shown <- function(chart, cl) {
    sc <- chart$x$series[[cl]]$scale
    gap <- sum(vapply(sc$breaks, function(b) b$end - b$start, numeric(1)))
    sc$end - sc$start - gap
  }
  for (chart in list(
    GC_chart(align_genes, cluster = "cluster", group = "name") %>%
      GC_scale(breaks = breaks) %>% GC_align("name", "x"),
    GC_chart(align_genes, cluster = "cluster", group = "name") %>%
      GC_align("name", "x") %>% GC_scale(breaks = breaks)
  )) {
    # both clusters show the same number of base pairs
    expect_equal(shown(chart, "1"), shown(chart, "2"))
    # left of x: 200 bp in cluster 1, and 500 bp less the 200 bp break in
    # cluster 2; right of x: 600 bp in cluster 1, 1100 bp in cluster 2
    expect_equal(shown(chart, "1"), 300 + 1100)
    expect_equal(unname(ranges(chart)[, "start"]), c(300 - 300, 1500 - 300 - 200))
    expect_equal(unname(ranges(chart)[, "end"]), c(300 + 1100, 1500 + 1100))
  }
})
