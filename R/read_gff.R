#' Read GFF Files
#'
#' This function reads GFF files from a specified directory or file path and
#' combines them into a single data frame.
#'
#' @param path A character string specifying the directory containing GFF files
#'   or the file path to a single GFF file.
#' @param fields An optional vector of character strings specifying the fields
#'   to extract from the GFF files.
#'
#' @return A data frame containing the combined data from the GFF files.
#'
#' @details This function can read multiple GFF files from a directory or a
#'   single GFF file. It processes each file, extracts the specified fields (if
#'   provided), adds a 'name' column with the filename, and combines the data
#'   frames from all files into one.
#'
#' @examples
#' \dontrun{
#' # Read GFF files from a directory
#' gff_data <- read_gff("path/to/directory")
#'
#' # Read a single GFF file
#' gff_data <- read_gff("path/to/file.gff")
#'
#' # Read specific fields from GFF files
#' gff_data <- read_gff(
#' "path/to/directory",
#' fields = c("seqid", "start", "end", "attributes")
#' )
#' }
#' @importFrom dplyr bind_rows
#' @importFrom utils read.table
#' @export
read_gff <- function(path, fields = NULL){

  if(dir.exists(path)){
    # It's a directory
    files <- list.files(path, pattern = "\\.gff3?$", full.names = TRUE)

    # Check if there are any .gff files in the directory
    if (length(files) == 0) {
      stop("No .gff files found in the specified directory.")
    }

    # Initialize an empty list to store data frames from each file
    data_list <- list()

    # Process each .gff file in the directory
    for (file in files) {
      data <- process_gff(file, fields)
      filename <- sub("\\.gff3?$", "", basename(file))
      data$filename <- filename
      data_list[[filename]] <- data
    }

    # Combine data frames from all files into one
    combined_data <- do.call(dplyr::bind_rows, data_list)

    return(combined_data)

  } else if(file.exists(path)){

    data <- process_gff(path, fields)

    return(data)

  } else {
    stop("The specified path does not exist.")
  }
}

#' @noRd
process_gff <- function(path, fields = NULL){

  field_names <- c("seqid", "source", "type", "start", "end", "score", "strand", "phase", "attributes")

  if(file.exists(path)){

    lines <- readLines(path)
    data <- read.table(
      text = lines,
      header = FALSE,
      quote = "",
      sep = "\t",
      fill = TRUE,
      stringsAsFactors = FALSE,
      col.names = field_names
      )

    data$attributes <- gsub("\\t.*", "", data$attributes)

    list_attributes <- lapply(data$attributes, parse_attributes)
    df_attributes <- dplyr::bind_rows(list_attributes, .id = "id")

    df_attributes <- tidyr::pivot_wider(df_attributes, names_from = .data$key, values_from = .data$value) %>%
      select(-.data$id)

    data <- cbind(data %>% dplyr::select(-attributes), df_attributes)

    if(!is.null(fields)){

      invalid_fields <- setdiff(fields, names(data))
      if (length(invalid_fields) > 0) {
        stop(paste("Invalid field(s):", paste(invalid_fields, collapse = ", ")))
      }

      data <- data %>% dplyr::select(dplyr::all_of(fields))
    }

  } else {
    stop("The specified path does not exist.")
  }

  return(data)
}

#' @noRd
parse_attributes <- function(info) {
  pairs <- strsplit(info, ";", fixed = TRUE)[[1]]  # Split the info string into pairs
  keys <- vector()   # Initialize empty vector for keys
  values <- vector() # Initialize empty vector for values

  for (pair in pairs) {
    parts <- strsplit(pair, "=", fixed = TRUE)[[1]]  # Split each pair at the '='
    if (length(parts) == 2) {
      keys <- c(keys, parts[1])   # Append key to keys vector
      values <- c(values, parts[2])  # Append value to values vector
    } else {
      keys <- c(keys, parts[1])   # Append key and assume missing value
      values <- c(values, NA)     # Append NA for the missing value
    }
  }

  # Create a data.frame from keys and values
  return(data.frame(key = keys, value = values, stringsAsFactors = FALSE))
}
