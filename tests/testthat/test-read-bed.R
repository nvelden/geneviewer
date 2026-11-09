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

test_that("BED12 files whose rows all have one block are read (B65)", {
  file <- tempfile(fileext = ".bed")
  on.exit(unlink(file))
  writeLines(c("chr1\t0\t100\ta\t0\t+\t0\t100\t0\t1\t100\t0",
               "chr1\t200\t300\tb\t0\t-\t200\t300\t0\t1\t100\t0"), file)
  bed <- read_bed(file)
  expect_equal(bed$blockSizes, list(100, 100))
  expect_equal(bed$blockStarts, list(1, 1))
  expect_equal(bed$chromStart, c(1, 201))
})
