test_that("adjacent and overlapping exons do not create introns", {
  data <- data.frame(start = c(1, 11, 15), end = c(10, 20, 30))
  result <- get_introns(data)
  expect_false(any(result$type == "intron"))
  expect_equal(result$start, data$start)
  expect_equal(result$end, data$end)
})

test_that("nested UTRs and exons use the full covered region", {
  data <- data.frame(start = c(201, 20, 1, 90, 40),
                     end = c(210, 30, 100, 120, 50),
                     type = c("exon", "UTR", "exon", "exon", "UTR"))
  result <- get_introns(data)
  introns <- result[result$type == "intron", ]
  expect_equal(introns$start, 121)
  expect_equal(introns$end, 200)
  original <- result[result$type != "intron", names(data)]
  expect_equal(original, data[order(data$start), ], ignore_attr = TRUE)
})

test_that("introns are computed independently per transcript and on both strands", {
  data <- data.frame(start = c(90, 1, 10, 31), end = c(80, 10, 1, 40),
                     transcript = c("reverse", "forward", "reverse", "forward"),
                     strand = c("-", "+", "-", "+"))
  result <- get_introns(data)
  introns <- result[result$type == "intron", ]
  expect_equal(introns$transcript, c("reverse", "forward"))
  expect_equal(introns$start, c(11, 11))
  expect_equal(introns$end, c(79, 30))
  expect_equal(introns$strand, c("-", "+"))
  expect_true(all(introns$start <= introns$end))
  # Reverse input exons retain their original orientation.
  expect_true(any(result$start == 90 & result$end == 80))
})

test_that("a one-base gap is retained and a single exon has no intron", {
  result <- get_introns(data.frame(start = c(1, 12), end = c(10, 20)))
  expect_equal(result$start[result$type == "intron"], 11)
  expect_equal(result$end[result$type == "intron"], 11)
  expect_false(any(get_introns(data.frame(start = 1, end = 10))$type == "intron"))
})
