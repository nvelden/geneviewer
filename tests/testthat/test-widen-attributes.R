test_that("reader metadata retains record and key order and missing fields", {
  metadata <- data.frame(
    id = c("2", "1", "2", "3", "1"),
    key = c("gene name", "Note", "Note", "gene name", "odd-name"),
    value = c("abc", NA, "", "xyz", "NA")
  )
  expect_identical(widen_attributes(metadata), dplyr::as_tibble(list(
    id = c("2", "1", "3"),
    `gene name` = c("abc", NA, "xyz"),
    Note = c("", NA, NA),
    `odd-name` = c(NA, "NA", NA)
  )))
})

test_that("repeated metadata keys retain all values in list columns", {
  metadata <- data.frame(
    id = c("2", "1", "2", "2", "3"),
    key = c("Note", "gene", "Note", "Note", "gene"),
    value = c("first", "abc", "second", "first", NA)
  )
  expect_warning(result <- widen_attributes(metadata), "output will contain list-cols")
  expect_identical(result, dplyr::as_tibble(list(
    id = c("2", "1", "3"),
    Note = list(c("first", "second", "first"), NULL, NULL),
    gene = list(NULL, "abc", NA_character_)
  )))
})

test_that("empty metadata and missing keys retain their previous shape", {
  metadata <- data.frame(id = character(), key = character(), value = character())
  expect_identical(widen_attributes(metadata), dplyr::as_tibble(list(id = character())))
  metadata <- data.frame(id = "1", key = NA_character_, value = "x")
  expect_identical(widen_attributes(metadata), dplyr::as_tibble(list(id = "1", `NA` = "x")))
})

test_that("metadata rejects column name collisions and empty names", {
  metadata <- data.frame(id = "1", key = "id", value = "x")
  expect_error(widen_attributes(metadata), "unique")
  metadata$key <- ""
  expect_error(widen_attributes(metadata), "empty")
  metadata <- data.frame(id = c("1", "2"), key = c(NA, "NA"), value = c("x", "y"))
  expect_error(widen_attributes(metadata), "unique")
})

test_that("GFF readers retain repeated attributes and rows without metadata", {
  file <- tempfile(fileext = ".gff3")
  on.exit(unlink(file))
  writeLines(c(
    "##gff-version 3",
    "chr1\tdemo\tgene\t1\t20\t.\t+\t.\tID=g1;Note=a;Note=b",
    "chr1\tdemo\tgene\t30\t40\t.\t+\t.\t.",
    "chr1\tdemo\tgene\t50\t60\t.\t-\t.\tID=g3;Note="
  ), file)
  expect_warning(result <- read_gff(file), "list-cols")
  expect_identical(result$start, c(1L, 30L, 50L))
  expect_identical(result$ID, list("g1", NULL, "g3"))
  expect_identical(result$Note, list(c("a", "b"), NULL, ""))
})

test_that("FASTA readers retain repeated keys, missing fields and sequences", {
  skip_if_not_installed("Biostrings")
  file <- tempfile(fileext = ".fasta")
  on.exit(unlink(file))
  writeLines(c(
    ">s1 [gene=a] [note=first] [note=second]", "MKT",
    ">s2 [gene=b]", "MAA"
  ), file)
  expect_warning(result <- read_fasta(file), "list-cols")
  expect_identical(result$gene, list("a", "b"))
  expect_identical(result$note, list(c("first", "second"), NULL))
  expect_identical(result$sequence, c("MKT", "MAA"))
})
