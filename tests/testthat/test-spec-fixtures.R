# The spec of every fixture of the JavaScript library: its call.R, evaluated
# with this package, gives the chart spec stored in its spec.json. The
# fixtures are read from a checkout of geneviewer-js (GENEVIEWER_JS_DIR); the
# test is skipped without it, except in the contract job in CI.

# These read files that are not in the repository, by paths in their call.R
# (fasta_files/, genbank_files/, gff_files/ and ~/Desktop/Bacillus/). They are
# named here so that any other fixture needing such a file fails in CI, and
# does not silently leave the contract.
local_data_fixtures <- c(
  "vig-LoadFastaFiles-1", "vig-LoadGFF-1", "vig-LoadGenBankFiles-1",
  "vig-LoadGenBankFiles-2", "vig-LoadGenBankFiles-3", "vig-MUMMER-1"
)

fixtures_dir <- function() {
  js_dir <- Sys.getenv("GENEVIEWER_JS_DIR")
  skip_unless(nzchar(js_dir), "GENEVIEWER_JS_DIR is not set")
  fixtures <- file.path(js_dir, "test", "fixtures")
  skip_unless(dir.exists(fixtures), "GENEVIEWER_JS_DIR has no test/fixtures")
  # The specs hold text such as "≥", written in UTF-8
  skip_unless(l10n_info()[["UTF-8"]], "needs a UTF-8 locale")
  fixtures
}

expect_fixture_spec <- function(fixtures, id) {
  env <- new.env(parent = globalenv())
  assign("%>%", magrittr::`%>%`, envir = env)
  set.seed(1)
  chart <- suppressWarnings(suppressMessages(
    source(file.path(fixtures, id, "call.R"), local = env, encoding = "UTF-8")$value
  ))
  expect_s3_class(chart, "geneviewer")
  expect_named(chart$x, "spec")
  spec <- as.character(htmlwidgets:::toJSON(htmlwidgets:::createPayload(chart)$x$spec))
  expected <- paste(readLines(file.path(fixtures, id, "spec.json"), warn = FALSE,
                              encoding = "UTF-8"), collapse = "\n")
  expect_identical(spec, expected, label = paste("spec of", id))
}

# Whether the files a fixture's call.R names under ~ exist here
has_local_data <- function(fixtures, id) {
  call <- readLines(file.path(fixtures, id, "call.R"), warn = FALSE, encoding = "UTF-8")
  paths <- unlist(regmatches(call, gregexpr('"~/[^"]+"', call)))
  all(file.exists(gsub('"', "", paths, fixed = TRUE)))
}

test_that("each fixture's call.R gives the fixture's spec", {
  fixtures <- fixtures_dir()
  ids <- list.dirs(fixtures, full.names = FALSE, recursive = FALSE)
  ids <- ids[file.exists(file.path(fixtures, ids, "spec.json"))]
  ids <- setdiff(ids, local_data_fixtures)
  expect_gt(length(ids), 0)

  for (id in ids) expect_fixture_spec(fixtures, id)
})

# Not a required test: the files are not there in CI
test_that("the fixtures that read local files give their specs, where the files exist", {
  fixtures <- fixtures_dir()
  expect_true(all(dir.exists(file.path(fixtures, local_data_fixtures))))

  here <- Filter(function(id) has_local_data(fixtures, id), local_data_fixtures)
  for (id in here) expect_fixture_spec(fixtures, id)

  missing <- setdiff(local_data_fixtures, here)
  if (length(missing) > 0) {
    skip(paste("files not in the repository:", paste(missing, collapse = ", ")))
  }
})
