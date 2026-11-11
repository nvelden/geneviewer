#' Read BED Files
#'
#' This function reads BED files from a specified directory or file path and
#' combines them into a single data frame. BED files use 0-based coordinate starts,
#' while this function transforms the coordinates to 1-based during import.
#' \code{chromStart} and \code{thickStart} are increased by 1; end positions
#' and \code{blockSizes} are kept as they are. \code{blockStarts} become
#' 1-based positions relative to \code{chromStart} (the first block starts at
#' 1), as in \code{rtracklayer::import.bed()}, so the start of a block is
#' \code{chromStart + blockStart - 1}.
#'
#' @param path A character string specifying the directory containing BED files
#'   or the file path to a single BED file.
#'
#' @return A data frame combining data from the BED files.
#'
#' @details This function can read multiple BED files from a directory or a
#'   single BED file from a specified path. It adds a 'filename' column with the
#'   name of the file, and combines the data frames from all files into one.
#'
#'   The columns `chrom`, `name`, `strand` and `itemRgb` are character, also
#'   when their values look like numbers (a chromosome named `1`), so that the
#'   files of a directory can be combined. `score` is numeric; a score of
#'   `"."` (no score) is `NA`.
#'
#' @examples
#' bed_file <- tempfile(fileext = ".bed")
#' writeLines(c(
#'   "chr1\t100\t500\tgeneA\t0\t+",
#'   "chr1\t700\t1200\tgeneB\t0\t-"
#' ), bed_file)
#'
#' # Read a single BED file
#' bed_data <- read_bed(bed_file)
#'
#' # Read all BED files in a directory
#' bed_data <- read_bed(dirname(bed_file))
#'
#' unlink(bed_file)
#' @importFrom dplyr bind_rows
#' @export
read_bed <- function(path){
  read_tables(path, "\\.bed[0-9]*$", "bed", process_bed)
}

#' @noRd
block_to_numeric <- function(x) {
  # A column of single blocks is read as numbers (B65)
  as.numeric(unlist(strsplit(as.character(x), ",")))
}

# Columns of BED files read as character; the others are guessed per file
bed_classes <- c(chrom = "character", name = "character", score = "character",
                 strand = "character", itemRgb = "character",
                 blockSizes = "character", blockStarts = "character")

#' @noRd
process_bed <- function(path){

  field_names <- c("chrom", "chromStart", "chromEnd", "name", "score",
                   "strand", "thickStart", "thickEnd", "itemRgb", "blockCount",
                   "blockSizes", "blockStarts")

  if(file.exists(path)){

    lines <- readLines(path)
    # Remove UCSC directives and comments wherever they appear. Words inside
    # chromosome or feature names are data, not declarations.
    header <- grepl("^[[:space:]]*(track|browser)([[:space:]]|$)|^[[:space:]]*(#|$)", lines)
    lines <- lines[!header]

    # Determine the number of columns from the first data line
    if (length(lines) > 0) {
      num_cols <- length(strsplit(lines[1], "\t")[[1]])
      used_fields <- field_names[1:num_cols]
    } else {
      used_fields <- field_names
    }

    data <- read.table(
      text = lines,
      header = FALSE,
      quote = "",
      sep = "\t",
      comment.char = "",
      fill = TRUE,
      stringsAsFactors = FALSE,
      col.names = used_fields,
      # Text by definition, also when it looks like a number (a chrom 1), so
      # that the files of a directory can be combined
      colClasses = bed_classes[intersect(names(bed_classes), used_fields)]
    )
    if ("score" %in% colnames(data)) {
      data$score <- score_to_numeric(data$score)
    }

    # Add +1 to starts (0-based to 1-based). BED ends are exclusive, so they
    # are already the 1-based end positions.
    if ("chromStart" %in% colnames(data)) {
    data$chromStart <- data$chromStart + 1
    }
    if ("thickStart" %in% colnames(data)) {
      data$thickStart <- data$thickStart + 1
    }
    # Convert blocks to a list of numeric vectors (one element per row)
    if ("blockSizes" %in% colnames(data)) {
      data$blockSizes <- lapply(data$blockSizes, block_to_numeric)
    }
    if ("blockStarts" %in% colnames(data)) {
      data$blockStarts <- lapply(data$blockStarts, function(x) block_to_numeric(x) + 1)
    }

  } else {
    stop("The specified path does not exist.")
  }

  return(data)
}
