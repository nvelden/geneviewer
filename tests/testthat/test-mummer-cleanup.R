mock_mummer_coords <- function(reference, query, maptype, seqtype, ...) {
  prefix <- file.path(dirname(query), paste0(
    tools::file_path_sans_ext(basename(reference)), "_",
    tools::file_path_sans_ext(basename(query)), "_", seqtype))
  writeLines("intermediate", paste0(prefix, ".delta"))
  writeLines("1\t100\t1\t100\t100\t100\t100\ta\tb",
             paste0(prefix, "_", gsub("-", "_", maptype), ".coords"))
}

test_that("default MUMmer output preserves unrelated and same-named files", {
  local_mocked_bindings(mummer = mock_mummer_coords)
  input <- tempfile("mummer-input-")
  dir.create(input)
  on.exit(unlink(input, recursive = TRUE), add = TRUE)
  stem <- basename(input)
  clusters <- paste0(stem, c("-a", "-b"))
  for (cl in clusters) writeLines(c(paste0(">", cl), "ACGT"), file.path(input, paste0(cl, ".fasta")))
  existing <- file.path(tempdir(), paste0(c(clusters[1], paste0(stem, "-unrelated")), ".fasta"))
  for (file in existing) writeLines("pre-existing data", file)
  on.exit(unlink(existing), add = TRUE)
  before <- list.files(tempdir(), pattern = "^geneviewer-mummer-", full.names = TRUE)

  result <- mummer_alignment(input, cluster = clusters, seqtype = "nucleotide")

  expect_equal(nrow(result), 1)
  for (file in existing) expect_identical(readLines(file), "pre-existing data")
  expect_true(all(file.exists(file.path(input, paste0(clusters, ".fasta")))))
  expect_identical(list.files(tempdir(), pattern = "^geneviewer-mummer-", full.names = TRUE), before)
})

test_that("MUMmer retains outputs in an isolated directory when requested", {
  local_mocked_bindings(mummer = mock_mummer_coords)
  root <- tempfile("mummer-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  for (cl in c("a", "b")) writeLines(c(paste0(">", cl), "ACGT"), file.path(root, paste0(cl, ".fasta")))
  writeLines("existing alignment", file.path(root, "a_b_nucleotide.delta"))

  mummer_alignment(root, seqtype = "nucleotide", output_dir = root, remove_files = FALSE)
  work <- list.dirs(root, recursive = FALSE)
  expect_length(work, 1)
  expect_true(file.exists(file.path(work, "a_b_nucleotide.delta")))
  expect_identical(readLines(file.path(root, "a_b_nucleotide.delta")), "existing alignment")
  expect_identical(readLines(file.path(root, "a.fasta")), c(">a", "ACGT"))
})

test_that("MUMmer cleans its working directory on validation and alignment errors", {
  root <- tempfile("mummer-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  writeLines(c(">a", "ACGT"), file.path(root, "a.fasta"))
  expect_error(mummer_alignment(root, cluster = c("a", "missing"), output_dir = root), "missing")
  expect_length(list.dirs(root, recursive = FALSE), 0)

  writeLines(c(">b", "ACGT"), file.path(root, "b.fasta"))
  local_mocked_bindings(mummer = function(...) stop("alignment failed"))
  expect_warning(result <- mummer_alignment(root, output_dir = root), "alignment failed")
  expect_null(result)
  expect_length(list.dirs(root, recursive = FALSE), 0)
  expect_true(all(file.exists(file.path(root, c("a.fasta", "b.fasta")))))
})
