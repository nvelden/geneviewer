test_that("reader metadata retains record and key order and missing fields", {
  metadata <- data.frame(
    id = c("2", "1", "2", "3", "1"),
    key = c("gene name", "Note", "Note", "gene name", "odd-name"),
    value = c("abc", NA, "", "xyz", "NA")
  )
  result <- widen_attributes(metadata)
  expect_identical(result$id, c("2", "1", "3"))
  expect_identical(result$data, dplyr::as_tibble(list(
    `gene name` = c("abc", NA, "xyz"),
    Note = c("", NA, NA),
    `odd-name` = c(NA, "NA", NA)
  )))
})

test_that("repeated metadata keys join their values with commas (B48)", {
  metadata <- data.frame(
    id = c("2", "1", "2", "2", "3", "3", "3"),
    key = c("Note", "gene", "Note", "Note", "gene", "Note", "Note"),
    value = c("first", "abc", "second", "first", NA, NA, NA)
  )
  expect_no_warning(result <- widen_attributes(metadata))
  expect_identical(result$id, c("2", "1", "3"))
  # Values in input order, missing values left out; all missing is NA
  expect_identical(result$data, dplyr::as_tibble(list(
    Note = c("first,second,first", NA, NA),
    gene = c(NA, "abc", NA)
  )))
})

test_that("empty metadata and missing keys retain their previous shape", {
  metadata <- data.frame(id = character(), key = character(), value = character())
  expect_identical(widen_attributes(metadata), list(id = character(), data = dplyr::as_tibble(list())))
  metadata <- data.frame(id = "1", key = NA_character_, value = "x")
  expect_identical(widen_attributes(metadata), list(id = "1", data = dplyr::as_tibble(list(`NA` = "x"))))
})

test_that("metadata id keys are separate from bookkeeping IDs", {
  metadata <- data.frame(id = "1", key = "id", value = "x")
  expect_identical(widen_attributes(metadata), list(id = "1", data = dplyr::as_tibble(list(id = "x"))))
})

test_that("metadata rejects duplicate normalized names and empty names", {
  metadata <- data.frame(id = "1", key = "", value = "x")
  expect_error(widen_attributes(metadata), "empty")
  metadata <- data.frame(id = c("1", "2"), key = c(NA, "NA"), value = c("x", "y"))
  expect_error(widen_attributes(metadata), "unique")
})

test_that("GFF attributes named id survive missing rows and field selection", {
  file <- tempfile(fileext = ".gff3")
  on.exit(unlink(file))
  writeLines(c("chr1\tdemo\tgene\t1\t20\t.\t+\t.\tid=custom1;ID=standard1",
               "chr1\tdemo\tgene\t30\t40\t.\t+\t.\t.",
               "chr1\tdemo\tgene\t50\t60\t.\t-\t.\tid=custom3"), file)
  result <- read_gff(file)
  expect_identical(result$id, c("custom1", NA_character_, "custom3"))
  expect_identical(result$ID, c("standard1", NA_character_, NA_character_))
  expect_identical(read_gff(file, fields = "id")$id, result$id)
})

test_that("FASTA metadata named id retains sequence order", {
  file <- tempfile(fileext = ".fasta")
  on.exit(unlink(file))
  writeLines(c(">s1 [id=custom1] [gene=a]", "MKT",
               ">s2 [gene=b]", "MAA",
               ">s3 [id=custom3] [gene=c]", "MAG"), file)
  result <- read_fasta(file)
  expect_identical(result$id, c("custom1", NA_character_, "custom3"))
  expect_identical(result$sequence, c("MKT", "MAA", "MAG"))
})

test_that("GFF readers join repeated attributes and keep rows without metadata", {
  file <- tempfile(fileext = ".gff3")
  on.exit(unlink(file))
  writeLines(c(
    "##gff-version 3",
    "chr1\tdemo\tgene\t1\t20\t.\t+\t.\tID=g1;Note=a;Note=b",
    "chr1\tdemo\tgene\t30\t40\t.\t+\t.\t.",
    "chr1\tdemo\tgene\t50\t60\t.\t-\t.\tID=g3;Note="
  ), file)
  expect_no_warning(result <- read_gff(file))
  expect_identical(result$start, c(1L, 30L, 50L))
  expect_identical(result$ID, c("g1", NA, "g3"))
  expect_identical(result$Note, c("a,b", NA, ""))
})

test_that("FASTA readers join repeated keys, keep missing fields and sequences", {
  file <- tempfile(fileext = ".fasta")
  on.exit(unlink(file))
  writeLines(c(
    ">s1 [gene=a] [note=first] [note=second]", "MKT",
    ">s2 [gene=b]", "MAA"
  ), file)
  expect_no_warning(result <- read_fasta(file))
  expect_identical(result$gene, c("a", "b"))
  expect_identical(result$note, c("first,second", NA))
  expect_identical(result$sequence, c("MKT", "MAA"))
})

test_that("read_gff() reads a repeated GFF3 key as text, other columns too (B48)", {
  file <- tempfile(fileext = ".gff3")
  on.exit(unlink(file))
  writeLines(c(
    "##gff-version 3",
    "chr1\tdemo\tgene\t100\t900\t.\t+\t.\tID=g1;Name=abcA;Dbxref=GeneID:1;Dbxref=UniProt:P1",
    "chr1\tdemo\tgene\t1000\t1900\t.\t-\t.\tID=g2;Name=abcB;Dbxref=GeneID:2",
    "chr1\tdemo\tgene\t2000\t2600\t.\t+\t.\tID=g3;Name=abcC"
  ), file)
  expect_no_warning(result <- read_gff(file))
  expect_identical(result$Name, c("abcA", "abcB", "abcC"))
  expect_identical(result$Dbxref, c("GeneID:1,UniProt:P1", "GeneID:2", NA))
  expect_false(any(vapply(result, is.list, logical(1))))
  # The table can be written and charted
  expect_no_error(utils::write.csv(result, tempfile()))
  expect_s3_class(GC_chart(result, group = "Name"), "htmlwidget")
})

test_that("read_gff() reads GTF and GFF2 attributes (B48)", {
  dir <- tempfile()
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE))
  file <- file.path(dir, "genes.gtf")
  writeLines(c(
    "chr1\tdemo\tgene\t100\t900\t.\t+\t.\tgene_id \"g1\"; gene_name \"abcA\";",
    "chr1\tdemo\texon\t100\t400\t.\t+\t0\tgene_id \"g1\"; transcript_id \"t1\"; tag \"basic\"; tag \"CCDS\";",
    "chr1\tdemo\tgene\t1000\t1900\t.\t-\t.\tgene_id \"g2\";  gene_name \"a b\"",
    "chr1\tdemo\tgene\t2000\t2600\t.\t+\t.\tgene_id g3; level 2; flag"
  ), file)
  result <- read_gff(file)
  expect_identical(names(result)[9:13], c("gene_id", "gene_name", "transcript_id", "tag", "level"))
  expect_identical(result$gene_id, c("g1", "g1", "g2", "g3"))
  expect_identical(result$transcript_id, c(NA, "t1", NA, NA))
  expect_identical(result$tag, c(NA, "basic,CCDS", NA, NA))
  # GFF2: values without quotes; a key without a value is NA
  expect_identical(result$level, c(NA, NA, NA, "2"))
  expect_identical(result$flag, c(NA, NA, NA, NA_character_))
  # Spaces inside quotes stay
  expect_identical(result$gene_name, c("abcA", NA, "a b", NA))
  expect_s3_class(GC_chart(result, group = "gene_name"), "htmlwidget")

  # Folders: .gtf and .gff2 files are read too
  file.copy(file, file.path(dir, "more.gff2"))
  folder <- read_gff(dir)
  expect_identical(sort(unique(folder$filename)), c("genes", "more"))
  expect_identical(nrow(folder), 8L)
  empty <- tempfile()
  dir.create(empty)
  expect_error(read_gff(empty), "No .gff or .gtf files found")
})
