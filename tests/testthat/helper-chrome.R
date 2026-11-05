# Opens a file in headless Chrome and returns the session, or skips the test
# when Chrome is missing or does not start (e.g. on a slow CI runner). Chrome
# is closed when the calling test ends.
chrome_page <- function(file, width = 992, height = 1323, env = parent.frame()) {
  skip_on_cran()
  skip_if_not_installed("chromote")
  skip_if(is.null(tryCatch(chromote::find_chrome(), error = function(e) NULL)), "Chrome not found")

  chrome <- tryCatch(chromote::Chromote$new(), error = function(e) NULL)
  skip_if(is.null(chrome), "Chrome could not be started")
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

# One page with D3 and the vendored library, kept for all tests
compile_page <- function() {
  skip_on_cran()
  skip_if_not_installed("chromote")
  skip_if(is.null(tryCatch(chromote::find_chrome(), error = function(e) NULL)), "Chrome not found")
  page <- getOption("geneviewer.test_compile_page")
  if (!is.null(page)) return(page)

  widgets <- system.file("htmlwidgets", package = "geneviewer")
  yaml <- readLines(file.path(widgets, "geneviewer.yaml"))
  core <- sub("^\\s*script:\\s*\\./", "", grep("geneviewer\\.core\\.js", yaml, value = TRUE))
  file <- tempfile(fileext = ".html")
  writeLines(c(
    "<!DOCTYPE html><html><head>",
    sprintf('<script src="file://%s"></script>', file.path(widgets, "lib/D3-7.8.5/d3.min.js")),
    sprintf('<script src="file://%s"></script>', file.path(widgets, core)),
    "</head><body></body></html>"
  ), file)
  page <- tryCatch(chromote::ChromoteSession$new(), error = function(e) NULL)
  skip_if(is.null(page), "Chrome could not be started")
  loaded <- page$Page$loadEventFired(wait_ = FALSE)
  page$Page$navigate(paste0("file://", normalizePath(file)), wait_ = FALSE)
  page$wait_for(loaded)
  options(geneviewer.test_compile_page = page)
  page
}
