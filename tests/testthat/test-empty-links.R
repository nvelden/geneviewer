test_that("empty alignments leave charts and their existing links unchanged", {
  genes <- data.frame(start = c(1, 20), end = c(10, 30), cluster = c("a", "b"))
  chart <- GC_chart(genes, cluster = "cluster")
  alignment <- data.frame(cluster1 = "a", cluster2 = "b", start1 = 1, end1 = 10,
                          start2 = 20, end2 = 30, identity = 90)
  empty <- alignment[FALSE, ]

  expect_identical(GC_links(chart, data = empty), chart)
  linked <- GC_links(chart, data = alignment)
  linked_model <- compile_chart(linked)
  expect_length(linked_model$links, 1)
  expect_identical(GC_links(linked, data = empty), linked)
  expect_identical(linked_model$links[[1]]$data$linkID, "0-1")

  # Adding a later nonempty alignment still gives it the next link-set ID.
  linked <- GC_links(GC_links(linked, data = empty), data = alignment)
  expect_identical(compile_chart(linked)$links[[2]]$data$linkID, "1-1")
})

test_that("GC_links without group or data says that one of them is needed", {
  genes <- data.frame(start = c(1, 50), end = c(40, 90), cluster = c(1, 2))
  expect_error(GC_links(GC_chart(genes, cluster = "cluster")),
               "Either 'group' or 'data' must be provided.", fixed = TRUE)
})

test_that("GC_links(group = ) reads the clusters of the chart, not another column named cluster (B70)", {
  genes <- data.frame(
    start = c(10, 100, 190, 10, 100, 190), end = c(80, 170, 260, 80, 170, 260),
    name = rep(c("abcA", "abcB", "abcC"), 2),
    genome = rep(c("genome1", "genome2"), each = 3)
  )
  expected <- (GC_chart(genes, cluster = "genome") %>% GC_links("name"))$x$spec$links$data

  values <- list("BGC1", c("genome1", "genome2", "genome2", "genome1", "genome1", "genome2"))
  for (value in values) {
    # The column before the chart's cluster column, and after it
    for (data in list(cbind(cluster = value, genes), cbind(genes, cluster = value))) {
      links <- (GC_chart(data, cluster = "genome") %>% GC_links("name"))$x$spec$links$data
      expect_identical(links, expected)
    }
  }
  expect_equal(expected$cluster1, rep("genome1", 3))
  expect_equal(expected$cluster2, rep("genome2", 3))

  # Links by that column itself cannot be named in the link table
  expect_error(GC_chart(cbind(cluster = "BGC1", genes), cluster = "genome") %>% GC_links("cluster"),
               "cannot be the group of the links")
})
