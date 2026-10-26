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
#' @examples
#' \dontrun{
#' # Read BED files from a directory
#' bed_data <- read_bed("path/to/directory")
#'
#' # Read a single BED file
#' bed_data <- read_bed("path/to/file.bed")
#' }
#' @importFrom dplyr bind_rows
#' @export
read_bed <- function(path){

  if(dir.exists(path)){
    # It's a directory
    files <- list.files(path, pattern = "\\.bed[0-9]*$", full.names = TRUE)

    # Check if there are any .bed files in the directory
    if (length(files) == 0) {
      stop("No .bed files found in the specified directory.")
    }

    # Initialize an empty list to store data frames from each file
    data_list <- list()

    # Process each .bed file in the directory
    for (file in files) {
      data <- process_bed(file)
      filename <- sub("\\.bed[0-9]*$", "", basename(file))
      data$filename <- filename
      data_list[[filename]] <- data
    }

    # Combine data frames from all files into one
    combined_data <- do.call(dplyr::bind_rows, data_list)

    return(combined_data)

  } else if(file.exists(path)){

    data <- process_bed(path)

    return(data)

  } else {
    stop("The specified path does not exist.")
  }
}

#' @noRd
block_to_numeric <- function(x) {
  as.numeric(unlist(strsplit(x, ",")))
}

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
      col.names = used_fields
    )

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
