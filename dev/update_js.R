# Vendor a release of the geneviewer JavaScript library
#
# Usage, from the package root:
#
#   Rscript dev/update_js.R 0.3.1
#
# Downloads the files the R package ships from the GitHub release
# v<version> of nvelden/geneviewer-js, checks them against the release's
# SHA256SUMS, and writes them to inst/htmlwidgets/lib/geneviewer-<version>/
# (the htmlwidgets binding, geneviewerwidget.js, to inst/htmlwidgets/).
# Then updates inst/htmlwidgets/geneviewer.yaml and removes the folder of the
# version vendored before.
#
# tests/testthat/test-vendored-js.R checks the vendored files against the
# vendored SHA256SUMS.

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 1 || !grepl("^[0-9]+\\.[0-9]+\\.[0-9]+(-[0-9A-Za-z.]+)?$", args)) {
  stop("Usage: Rscript dev/update_js.R <version>, e.g. 0.3.1", call. = FALSE)
}
version <- args[[1]]
if (!file.exists("DESCRIPTION") || !dir.exists("inst/htmlwidgets")) {
  stop("Run this from the root of the geneviewer R package", call. = FALSE)
}

files <- c("geneviewer.core.js", "geneviewer.css", "themes.json", "geneviewerwidget.js")
release <- sprintf("https://github.com/nvelden/geneviewer-js/releases/download/v%s", version)

sha256 <- function(path) {
  if (exists("sha256sum", envir = asNamespace("tools"))) {
    return(unname(tools::sha256sum(path)))
  }
  if (requireNamespace("digest", quietly = TRUE)) {
    return(vapply(path, digest::digest, character(1), algo = "sha256", file = TRUE,
                  USE.NAMES = FALSE))
  }
  stop("Needs R 4.5 or later, or the digest package", call. = FALSE)
}

# Download into a temporary folder; nothing in the package changes until all
# files are checked
download <- file.path(tempfile("geneviewer-js-"))
dir.create(download)
for (file in c(files, "SHA256SUMS")) {
  url <- paste0(release, "/", file)
  message("Downloading ", url)
  status <- utils::download.file(url, file.path(download, file), mode = "wb", quiet = TRUE)
  if (status != 0) stop("Download failed: ", url, call. = FALSE)
}

# SHA256SUMS lists every file of the release, as "<hash>  <file>"
sums <- readLines(file.path(download, "SHA256SUMS"))
sums <- sums[nzchar(sums)]
expected <- stats::setNames(sub("^([0-9a-f]{64}) +\\*?(.+)$", "\\1", sums),
                            sub("^([0-9a-f]{64}) +\\*?(.+)$", "\\2", sums))
missing <- setdiff(files, names(expected))
if (length(missing) > 0) {
  stop("SHA256SUMS does not list: ", paste(missing, collapse = ", "), call. = FALSE)
}
actual <- sha256(file.path(download, files))
bad <- files[actual != expected[files]]
if (length(bad) > 0) {
  stop("Checksum mismatch: ", paste(bad, collapse = ", "), call. = FALSE)
}
message("Checksums OK: ", paste(files, collapse = ", "))

# Write the files
lib <- file.path("inst/htmlwidgets/lib", paste0("geneviewer-", version))
old <- setdiff(list.files("inst/htmlwidgets/lib", pattern = "^geneviewer-", full.names = TRUE), lib)
dir.create(lib, showWarnings = FALSE)
for (file in setdiff(files, "geneviewerwidget.js")) {
  stopifnot(file.copy(file.path(download, file), file.path(lib, file), overwrite = TRUE))
}
stopifnot(
  file.copy(file.path(download, "SHA256SUMS"), file.path(lib, "SHA256SUMS"), overwrite = TRUE),
  file.copy(file.path(download, "geneviewerwidget.js"), "inst/htmlwidgets/geneviewerwidget.js",
            overwrite = TRUE)
)

# The dependencies of the widget
yaml <- c(
  "dependencies:",
  "  - name: geneviewerwidget",
  sprintf("    version: %s", version),
  "    src: htmlwidgets",
  "    script: geneviewerwidget.js",
  "  - name: geneviewer",
  sprintf("    version: %s", version),
  "    src: htmlwidgets",
  sprintf("    script: ./lib/geneviewer-%s/geneviewer.core.js", version),
  sprintf("    stylesheet: ./lib/geneviewer-%s/geneviewer.css", version),
  "  - name: D3",
  "    version: 7.8.5",
  "    src: htmlwidgets",
  "    script: ./lib/D3-7.8.5/d3.min.js"
)
writeLines(yaml, "inst/htmlwidgets/geneviewer.yaml")

if (length(old) > 0) {
  unlink(old, recursive = TRUE)
  message("Removed ", paste(old, collapse = ", "))
}
message("Vendored geneviewer-js ", version, " in ", lib)
