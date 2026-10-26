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
#' gff_file <- system.file("extdata", "HQ113105.1.gff3", package = "geneviewer")
#'
#' # Read a single GFF file
#' gff_data <- read_gff(gff_file)
#'
#' # Read GFF files from a directory
#' gff_data <- read_gff(dirname(gff_file))
#'
#' # Read specific fields
#' gff_data <- read_gff(
#'   gff_file,
#'   fields = c("seqid", "type", "start", "end", "strand", "Name")
#' )
#' @importFrom dplyr bind_rows
#' @importFrom utils read.table
#' @export
read_gff <- function(path, fields = NULL){

  data <- read_tables(path, "\\.gff3?$", "gff", function(file) process_gff(file, fields))

  if(!is.null(fields)){
    invalid_fields <- setdiff(fields, names(data))
    if (length(invalid_fields) > 0) {
      stop(paste("Invalid field(s):", paste(invalid_fields, collapse = ", ")))
    }
    # Files of a directory keep the column with their name
    columns <- if (dir.exists(path)) c(fields, "filename") else fields
    data <- data %>% dplyr::select(dplyr::all_of(columns))
  }

  return(data)
}

#' @noRd
process_gff <- function(path, fields = NULL){

  field_names <- c("seqid", "source", "type", "start", "end", "score", "strand", "phase", "attributes")

  if(file.exists(path)){

    lines <- readLines(path)
    # Stop at the FASTA section (starts with ##FASTA or a line starting with >)
    fasta_start <- grep("^(##FASTA|>)", lines)
    if (length(fasta_start) > 0) {
      lines <- lines[seq_len(fasta_start[1] - 1)]
    }
    # Drop comment and directive lines; '#' inside a line is kept as data
    lines <- lines[!grepl("^#", lines)]
    data <- read.table(
      text = lines,
      header = FALSE,
      quote = "",
      sep = "\t",
      fill = TRUE,
      comment.char = "",
      stringsAsFactors = FALSE,
      col.names = field_names
      )

    data$attributes <- gsub("\\t.*", "", data$attributes)

    list_attributes <- lapply(data$attributes, parse_attributes)
    df_attributes <- dplyr::bind_rows(list_attributes, .id = "id")

    if (nrow(df_attributes) > 0) {
      metadata <- widen_attributes(df_attributes)
      # Bookkeeping IDs are separate from user attributes (which may include id).
      df_attributes <- metadata$data[match(seq_len(nrow(data)), as.integer(metadata$id)), ]
      data <- cbind(data %>% dplyr::select(-attributes), df_attributes)
    } else {
      data <- data %>% dplyr::select(-attributes)
    }

  } else {
    stop("The specified path does not exist.")
  }

  return(data)
}

#' @noRd
parse_attributes <- function(info) {
  # No attributes: missing column, empty or "."
  if (is.na(info) || trimws(info) %in% c("", ".")) {
    return(data.frame(key = character(0), value = character(0), stringsAsFactors = FALSE))
  }

  pairs <- trimws(strsplit(info, ";", fixed = TRUE)[[1]], which = "left")
  pairs <- pairs[pairs != ""]

  # Split each pair at the first '='; a key without '=' gets NA
  has_value <- grepl("=", pairs, fixed = TRUE)
  keys <- trimws(ifelse(has_value, sub("=.*$", "", pairs), pairs))
  values <- ifelse(has_value, url_decode(sub("^[^=]*=", "", pairs)), NA_character_)

  return(data.frame(key = keys, value = values, stringsAsFactors = FALSE))
}

#' @noRd
url_decode <- function(x) {
  vapply(x, function(value) {
    if (!grepl("%[0-9A-Fa-f]{2}", value)) {
      return(value)
    }
    # Keep a literal '%' that is not followed by two hex digits
    value <- gsub("%(?![0-9A-Fa-f]{2})", "%25", value, perl = TRUE)
    decoded <- utils::URLdecode(value)
    Encoding(decoded) <- "UTF-8"
    decoded
  }, character(1), USE.NAMES = FALSE)
}
