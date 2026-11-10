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

test_that("GC_legend stores and checks groupStyle", {
  chart <- GC_chart(group_genes, group = "class") %>%
    GC_legend(groupStyle = list(A = list(fontStyle = "italic",
                                         marker = list(stroke = "red"))))
  model <- compile_chart(chart)
  expect_equal(model$legend$groupStyle$A$fontStyle, "italic")
  expect_equal(model$legend$groupStyle$A$marker$stroke, "red")
  expect_null(compile_chart((GC_chart(group_genes, group = "class") %>% GC_legend()))$legend$groupStyle)
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
  options <- compile_chart(chart)$series[["tx1"]]$transcript
  expect_equal(options$groupStyleExons, list(tx1 = list(fill = "red")))
  expect_equal(options$groupStyleIntrons, list(tx1 = list(stroke = "blue")))
  expect_equal(options$groupStyleUTRs, list(tx2 = list(fill = "green")))
  expect_equal(options$styleBy, "transcript")
  # left out when not given
  plain <- GC_chart(transcripts) %>% GC_transcript(transcript = "transcript", type = "type")
  plain_model <- compile_chart(plain)
  expect_null(plain_model$series[["tx1"]]$transcript$groupStyleExons)
  expect_null(plain_model$series[["tx1"]]$transcript$styleBy)
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
  options <- compile_chart(chart)$series[[1]]$coordinates
  expect_equal(options$groupStyle, list(B = list(show = FALSE)))
  # the group of GC_chart(), unless style_by is given
  expect_equal(options$styleBy, "class")
  by_family <- GC_chart(group_genes, group = "class") %>%
    GC_coordinates(style_by = "family", groupStyle = list(x = list(fill = "red")))
  expect_equal(compile_chart(by_family)$series[[1]]$coordinates$styleBy, "family")
  # nothing is stored without groupStyle
  plain <- GC_chart(group_genes, group = "class") %>% GC_coordinates()
  expect_false(any(c("groupStyle", "styleBy") %in% names(compile_chart(plain)$series[[1]]$coordinates)))
  # a groupStyle needs a column to match against
  expect_error(GC_chart(group_genes[, c("start", "end")]) %>%
                 GC_coordinates(groupStyle = list(A = list(show = FALSE))), "style_by")
  expect_error(GC_chart(group_genes, group = "class") %>%
                 GC_coordinates(groupStyle = list(A = TRUE)), "named list")
  expect_error(GC_chart(group_genes, group = "class") %>%
                 GC_coordinates(style_by = "nope"), "single column name")
})
