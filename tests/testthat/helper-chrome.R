# Whether this run must run every test that needs a prerequisite: set by the
# contract job in CI, where a missing prerequisite is a failure, not a skip
tests_required <- function() {
  isTRUE(as.logical(Sys.getenv("GENEVIEWER_REQUIRE_TESTS")))
}

# Skips the test unless ok; fails instead where the tests are required
skip_unless <- function(ok, reason) {
  if (isTRUE(ok)) {
    return(invisible(TRUE))
  }
  if (tests_required()) stop(reason, call. = FALSE)
  skip(reason)
}

skip_unless_chrome <- function() {
  if (!tests_required()) skip_on_cran()
  skip_unless(requireNamespace("chromote", quietly = TRUE), "chromote cannot be loaded")
  skip_unless(!is.null(tryCatch(chromote::find_chrome(), error = function(e) NULL)),
              "Chrome not found")
}

# Keeps the error of a browser that does not start (e.g. on a slow CI runner):
# in the skip's reason, or as the error where the tests are required
chrome_error <- function(e) {
  if (tests_required()) stop(e)
  skip(paste("Chrome could not be started:", conditionMessage(e)))
}

# A browser, tried a few times: about one start in a hundred fails on its own
# ("cannot open the connection"), also on an idle machine
start_chrome <- function(attempts = 3) {
  for (attempt in seq_len(attempts)) {
    chrome <- tryCatch(suppressWarnings(chromote::Chromote$new()), error = identity)
    if (!inherits(chrome, "error")) return(chrome)
  }
  chrome_error(chrome)
}

# Opens a file in headless Chrome and returns the session, or skips the test
# when Chrome is missing or does not start. Chrome is closed when the calling
# test ends.
chrome_page <- function(file, width = 992, height = 1323, env = parent.frame()) {
  skip_unless_chrome()

  chrome <- start_chrome()
  do.call(on.exit, list(substitute(chrome$close(), list(chrome = chrome)), add = TRUE),
          envir = env)

  session <- chrome$new_session(width = width, height = height)
  loaded <- session$Page$loadEventFired(wait_ = FALSE)
  session$Page$navigate(paste0("file://", normalizePath(file)), wait_ = FALSE)
  session$wait_for(loaded)
  session
}

# The render model the library draws a chart from: geneviewer.compile() of
# the spec the widget sends, run with the vendored library in headless Chrome.
# Tests of results that R computed before the library (scales, recycling per
# cluster, themes, ...) check it. Skipped where Chrome is not available.
# Arrays of values become vectors, arrays of objects lists, objects named
# lists; the data of a cluster is a list of columns. With simplify = FALSE
# every array is a list, to tell a one-value array from a value.
compile_chart <- function(chart, simplify = TRUE) {
  page <- compile_page()
  spec <- as.character(htmlwidgets:::toJSON(htmlwidgets:::createPayload(chart)$x$spec))
  result <- page$Runtime$evaluate(paste0("JSON.stringify(geneviewer.compile(", spec, "))"))
  if (!is.null(result$exceptionDetails)) {
    stop("compile() failed: ", result$exceptionDetails$exception$description)
  }
  jsonlite::fromJSON(result$result$value, simplifyVector = simplify,
                     simplifyDataFrame = FALSE, simplifyMatrix = FALSE)
}

# The browser, page and file of compile_page(), for one test run: setup-chrome.R
# closes them when the run ends
compile_session <- new.env(parent = emptyenv())

# One page with D3 and the vendored library, kept for the tests of a run and
# opened again if it was closed
compile_page <- function() {
  skip_unless_chrome()
  page <- compile_session$page
  if (!is.null(page) && page$is_active() && compile_session$chrome$is_alive()) {
    return(page)
  }
  close_compile_page()

  widgets <- system.file("htmlwidgets", package = "geneviewer")
  yaml <- readLines(file.path(widgets, "geneviewer.yaml"))
  core <- sub("^\\s*script:\\s*\\./", "", grep("geneviewer\\.core\\.js", yaml, value = TRUE))
  compile_session$file <- tempfile(fileext = ".html")
  writeLines(c(
    "<!DOCTYPE html><html><head>",
    sprintf('<script src="file://%s"></script>', file.path(widgets, "lib/D3-7.8.5/d3.min.js")),
    sprintf('<script src="file://%s"></script>', file.path(widgets, core)),
    "</head><body></body></html>"
  ), compile_session$file)

  compile_session$chrome <- start_chrome()
  page <- tryCatch(compile_session$chrome$new_session(), error = chrome_error)
  loaded <- page$Page$loadEventFired(wait_ = FALSE)
  page$Page$navigate(paste0("file://", normalizePath(compile_session$file)), wait_ = FALSE)
  page$wait_for(loaded)
  compile_session$page <- page
  page
}

close_compile_page <- function() {
  if (!is.null(compile_session$chrome)) try(compile_session$chrome$close(), silent = TRUE)
  if (!is.null(compile_session$file)) unlink(compile_session$file)
  rm(list = ls(compile_session), envir = compile_session)
  invisible()
}
