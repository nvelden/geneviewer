# The JavaScript vendored by dev/update_js.R is the released files

sha256_file <- function(path) {
  if (exists("sha256sum", envir = asNamespace("tools"))) {
    return(unname(get("sha256sum", envir = asNamespace("tools"))(path)))
  }
  vapply(path, digest::digest, character(1), algo = "sha256", file = TRUE, USE.NAMES = FALSE)
}

test_that("the vendored JavaScript matches the release's SHA256SUMS", {
  if (!exists("sha256sum", envir = asNamespace("tools"))) skip_if_not_installed("digest")

  widgets <- system.file("htmlwidgets", package = "geneviewer")
  yaml <- readLines(file.path(widgets, "geneviewer.yaml"))
  script <- grep("geneviewer-[^/]+/geneviewer\\.core\\.js", yaml, value = TRUE)
  expect_length(script, 1)
  lib <- file.path(widgets, dirname(sub("^\\s*script:\\s*\\./", "", script)))

  sums <- readLines(file.path(lib, "SHA256SUMS"))
  sums <- sums[nzchar(sums)]
  expected <- stats::setNames(sub("^([0-9a-f]{64}) +\\*?(.+)$", "\\1", sums),
                              sub("^([0-9a-f]{64}) +\\*?(.+)$", "\\2", sums))

  files <- c(
    geneviewer.core.js = file.path(lib, "geneviewer.core.js"),
    geneviewer.css = file.path(lib, "geneviewer.css"),
    themes.json = file.path(lib, "themes.json"),
    geneviewerwidget.js = file.path(widgets, "geneviewerwidget.js")
  )
  expect_true(all(file.exists(files)))
  expect_true(all(names(files) %in% names(expected)))
  expect_identical(sha256_file(unname(files)), unname(expected[names(files)]))
})

test_that("the widget loads the vendored files", {
  widgets <- system.file("htmlwidgets", package = "geneviewer")
  yaml <- readLines(file.path(widgets, "geneviewer.yaml"))
  paths <- sub("^\\s*(script|stylesheet):\\s*", "", grep("^\\s*(script|stylesheet):", yaml, value = TRUE))
  expect_true(all(file.exists(file.path(widgets, paths))))
})
