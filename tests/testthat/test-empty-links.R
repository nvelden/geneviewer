test_that("empty alignments leave charts and their existing links unchanged", {
  genes <- data.frame(start = c(1, 20), end = c(10, 30), cluster = c("a", "b"))
  chart <- GC_chart(genes, cluster = "cluster")
  alignment <- data.frame(cluster1 = "a", cluster2 = "b", start1 = 1, end1 = 10,
                          start2 = 20, end2 = 30, identity = 90)
  empty <- alignment[FALSE, ]

  expect_identical(GC_links(chart, data = empty), chart)
  linked <- GC_links(chart, data = alignment)
  expect_length(linked$x$links, 1)
  expect_identical(GC_links(linked, data = empty), linked)
  expect_identical(linked$x$links[[1]]$data$linkID, "0-1")

  # Adding a later nonempty alignment still gives it the next link-set ID.
  linked <- GC_links(GC_links(linked, data = empty), data = alignment)
  expect_identical(linked$x$links[[2]]$data$linkID, "1-1")
})
