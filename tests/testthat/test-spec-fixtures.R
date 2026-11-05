# The spec of every fixture of the JavaScript library: its call.R, evaluated
# with this package, gives the chart spec stored in its spec.json. The
# fixtures are read from a checkout of geneviewer-js (GENEVIEWER_JS_DIR); the
# test is skipped without it, except in the contract job in CI.

test_that("each fixture's call.R gives the fixture's spec", {
  js_dir <- Sys.getenv("GENEVIEWER_JS_DIR")
  skip_unless(nzchar(js_dir), "GENEVIEWER_JS_DIR is not set")
  fixtures <- file.path(js_dir, "test", "fixtures")
  skip_unless(dir.exists(fixtures), "GENEVIEWER_JS_DIR has no test/fixtures")
  # The specs hold text such as "≥", written in UTF-8
  skip_unless(l10n_info()[["UTF-8"]], "needs a UTF-8 locale")

  ids <- list.dirs(fixtures, full.names = FALSE, recursive = FALSE)
  ids <- ids[file.exists(file.path(fixtures, ids, "spec.json"))]
  expect_gt(length(ids), 0)

  for (id in ids) {
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
})
