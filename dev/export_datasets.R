# Export the example datasets for the Python package (geneviewer-py)
#
# Usage, from the package root:
#
#   Rscript dev/export_datasets.R <geneviewer-py>/src/geneviewer/datasets
#
# Writes every dataset in data/ as <name>.csv (UTF-8, text quoted, a missing
# value as NA; an empty field is an empty string), and datasets.json with, for each dataset, its
# title (from man/<name>.Rd), its number of rows and its columns with their
# R type: character, integer or numeric.
#
# Numbers are written with 15 significant digits, so a value such as
# 51.800000000000004 (0.518 * 100) is written as 51.8. Each file is read
# again and compared with the dataset (numbers to 14 digits); the script
# stops when they differ.

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 1 || !dir.exists(args[[1]])) {
  stop("Usage: Rscript dev/export_datasets.R <geneviewer-py>/src/geneviewer/datasets", call. = FALSE)
}
out <- normalizePath(args[[1]])

number_text <- function(x) {
  ifelse(is.na(x), NA_character_, formatC(x, digits = 15, format = "g", width = 1))
}

rd_title <- function(name) {
  rd <- tools::parse_Rd(file.path("man", paste0(name, ".Rd")))
  tags <- vapply(rd, attr, "", "Rd_tag")
  trimws(paste(unlist(rd[[which(tags == "\\title")]]), collapse = ""))
}

index <- list()
for (file in sort(list.files("data", pattern = "\\.rda$", full.names = TRUE))) {
  env <- new.env()
  for (name in load(file, envir = env)) {
    data <- as.data.frame(env[[name]], stringsAsFactors = FALSE)
    types <- vapply(data, function(x) class(x)[1], "")
    if (!all(types %in% c("character", "integer", "numeric"))) {
      stop(name, ": a column is not character, integer or numeric", call. = FALSE)
    }
    if (any(vapply(data, function(x) is.character(x) && any(x == "NA", na.rm = TRUE), TRUE))) {
      stop(name, ": the text \"NA\" would be read as a missing value", call. = FALSE)
    }
    text <- data
    text[types == "numeric"] <- lapply(data[types == "numeric"], number_text)
    csv <- file.path(out, paste0(name, ".csv"))
    utils::write.csv(text, csv, row.names = FALSE, na = "NA", quote = which(types == "character"),
                     fileEncoding = "UTF-8", eol = "\n")

    again <- utils::read.csv(csv, colClasses = unname(types), na.strings = "NA", check.names = FALSE,
                             encoding = "UTF-8", stringsAsFactors = FALSE)
    same <- identical(names(again), names(data)) &&
      identical(vapply(again, function(x) class(x)[1], ""), types) &&
      identical(lapply(again[types != "numeric"], as.vector), lapply(data[types != "numeric"], as.vector)) &&
      isTRUE(all.equal(again, data, tolerance = 1e-14, check.attributes = FALSE))
    if (!same) stop(name, ": the file does not give the dataset back", call. = FALSE)

    index[[name]] <- list(
      title = rd_title(name),
      rows = nrow(data),
      columns = lapply(names(data), function(column) list(name = column, type = types[[column]]))
    )
    cat(sprintf("%s: %d rows, %d columns\n", name, nrow(data), ncol(data)))
  }
}

writeLines(jsonlite::toJSON(index, auto_unbox = TRUE, pretty = TRUE),
           file.path(out, "datasets.json"), useBytes = TRUE)
