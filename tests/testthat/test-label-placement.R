# Genes of two clusters: wide genes, and narrow genes that crowd near the
# right end, with labels that overlap
placement_genes <- data.frame(
  cluster = rep(c("a", "b"), each = 6),
  start = rep(c(1, 400, 800, 830, 860, 890), 2),
  end = rep(c(380, 780, 828, 858, 888, 1000), 2),
  name = rep(c("first gene", "second gene", "x long name", "y long name",
               "z long name", "last long name"), 2),
  homolog = rep(paste0("h", 1:6), 2),
  identity = c(rep(NA, 6), 91.25, 64, 80, 55, 70, 99)
)

placement_chart <- function(...) {
  GC_chart(placement_genes, cluster = "cluster", height = "260px", width = "700px") %>%
    GC_labels("name", ...)
}

test_that("GC_links() and GC_labels() check where labels go", {
  expect_error(GC_links(placement_chart(), group = "homolog", label_position = "middle"),
               "'label_position' must be one of")
  expect_error(placement_chart(adjustLabels = "shrink"),
               "'adjustLabels' must be TRUE, FALSE, \"hide\" or \"spread\"")
  expect_error(placement_chart(adjustLabels = "spread", leaderLine = "grey"),
               "'leaderLine' must be FALSE or a named list")
  expect_error(placement_chart(adjustLabels = "spread", leaderLine = list(length = "30px")),
               "'leaderLine\\$length' must be a single number")
  chart <- GC_links(placement_chart(), group = "homolog", label_position = "link")
  expect_identical(compile_chart(chart)$links[[1]]$options$labelPosition, "link")
  # Links from data keep their labels when they go on the links
  alignment <- data.frame(cluster1 = "a", cluster2 = "b", start1 = 1, end1 = 380,
                          start2 = 1, end2 = 380, identity = 90)
  expect_false(compile_chart(GC_links(placement_chart(), data = alignment))$links[[1]]$options$label)
  expect_true(compile_chart(GC_links(placement_chart(), data = alignment,
                       label_position = "link"))$links[[1]]$options$label)
  expect_error(placement_chart(adjustLabels = "spread", spreadPosition = "left"),
               "'spreadPosition' must be \"bottom\" or \"top\"")
})
