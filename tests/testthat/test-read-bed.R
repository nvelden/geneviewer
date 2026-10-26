test_that("BED feature names containing track preserve all records", {
  file <- tempfile(fileext = ".bed")
  on.exit(unlink(file))
  writeLines(c("chr1\t0\t10\tfirst", "chr1\t20\t30\ttrack_gene",
               "chr1\t40\t50\tlast#1", "track_chr\t60\t70\tbrowser_gene"), file)
  bed <- read_bed(file)
  expect_equal(bed$name, c("first", "track_gene", "last#1", "browser_gene"))
  expect_equal(bed$chromStart, c(1, 21, 41, 61))
  expect_equal(bed$chromEnd, c(10, 30, 50, 70))
})

test_that("BED directives and comments do not drop earlier tracks", {
  file <- tempfile(fileext = ".bed")
  on.exit(unlink(file))
  writeLines(c("# BED example", "browser position chr1:1-100", "",
               "track name=first", "chr1\t0\t10\tfirst",
               "  # another track", "track name=second",
               "chr1\t20\t30\tsecond", "browser hide all"), file)
  bed <- read_bed(file)
  expect_equal(bed$name, c("first", "second"))
  expect_equal(bed$chromStart, c(1, 21))
})
