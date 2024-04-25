#' Read BED Files
#'
#' This function reads BED files from a specified directory or file path and
#' combines them into a single data frame. BED files use 0-based coordinate starts,
#' while this function transforms the coordinates to 1-based during import.
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
  as.numeric(unlist(strsplit(x, ","))) + 1
}

#' @noRd
process_bed <- function(path){

  field_names <- c("chrom", "chromStart", "chromEnd", "name", "score",
                   "strand", "thickStart", "thickEnd", "itemRgb", "blockCount",
                   "blockSizes", "blockStarts")

  if(file.exists(path)){

    lines <- readLines(path)
    # Find the line with the pattern 'track'
    track_line <- grep("track", lines)
    # Read data from the line after the 'track' line if it exists
    if (length(track_line) > 0) {
      lines <- lines[(track_line + 1):length(lines)]
    }

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
      fill = TRUE,
      stringsAsFactors = FALSE,
      col.names = used_fields
    )

    # Add +1 to Start and End
    if ("chromStart" %in% colnames(data)) {
    data$chromStart <- data$chromStart + 1
    }
    if("chromEnd" %in% colnames(data)){
    data$chromEnd <- data$chromEnd + 1
    }
    if ("thickStart" %in% colnames(data)) {
      data$thickStart <- data$thickStart + 1
    }
    if("thickEnd" %in% colnames(data)){
      data$thickEnd <- data$thickEnd + 1
    }
    # Convert blocks to numeric vector
    if ("blockSizes" %in% colnames(data)) {
      data$blockSizes <- sapply(data$blockSizes, block_to_numeric)
    }
    if ("blockStarts" %in% colnames(data)) {
      data$blockStarts <- sapply(data$blockStarts, block_to_numeric)
    }

  } else {
    stop("The specified path does not exist.")
  }

  return(data)
}
