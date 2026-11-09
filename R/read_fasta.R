#'Read Protein Sequences from FASTA Files
#'
#'This function reads protein sequences from the specified FASTA file or all
#'FASTA files within a directory. It specifically looks for metadata in the
#'FASTA headers with key-value pairs separated by an equals sign `=`. For
#'example, from the header '>protein1 \[gene=scnD\] \[protein=ScnD\]', it extracts
#''gene' as the key and 'scnD' as its value, and similarly for other key-value
#'pairs.
#'
#'The files are read with base R, so no Bioconductor packages are needed.
#'Gzip-compressed files are read as well.
#'
#'@param fasta_path Path to the FASTA file or directory containing FASTA files.
#'@param keys An optional vector of strings representing specific keys within
#'  the fasta header to retain in the final data frame. If `NULL` (the default),
#'  all keys within the specified feature are included.
#'@param sequence Logical; if `TRUE`, the protein sequences are included in the
#'  returned data frame.
#'@param file_extension Extension of the FASTA files to be read from the
#'  directory (default is 'fasta').
#'@return A data frame with columns for each piece of information extracted from
#'  the FASTA headers.
#'
#'@importFrom dplyr bind_rows select any_of
#'@importFrom tools file_path_sans_ext
#'@importFrom rlang .data
#' @examples
#' fasta_file <- system.file("extdata", "BGC0000108.fasta", package = "geneviewer")
#'
#' # Read sequences from a single FASTA file
#' sequences_df <- read_fasta(fasta_file)
#'
#' # Read all FASTA files in a directory, without the sequences
#' sequences_df <- read_fasta(dirname(fasta_file), sequence = FALSE)
#'@export
read_fasta <- function(fasta_path, sequence = TRUE, keys = NULL, file_extension = "fasta") {

  # Initialize an empty list to store data frames from each file
  all_dfs <- list()

  # Check if the path is a directory or a single file
  if (dir.exists(fasta_path)) {
    # It's a directory, get all FASTA files in the directory
    file_pattern <- paste0("\\.", file_extension, "$")
    fasta_files <- list.files(fasta_path, pattern = file_pattern, full.names = TRUE)
  } else if (file.exists(fasta_path)) {
    # It's a single file
    fasta_files <- list(fasta_path)
  } else {
    stop("Provided path does not exist or is not a valid FASTA file or directory.")
  }

  # Process each FASTA file
  for (file_path in fasta_files) {
    tryCatch({
      # Extract the file name (without the path and extension)
      cluster_name <- tools::file_path_sans_ext(basename(file_path))

      # Read the headers and sequences from the file
      records <- read_fasta_records(file_path)
      fasta_headers <- records$headers
      sequences <- records$sequences

      # Find matches

      # Key-value pairs: "[key=value]" (the value can contain spaces and
      # brackets; it ends at the "]" before the next "[key=" or the end), or
      # without brackets "key=value" running up to the next " key=" or the end
      bracket_pattern <- "\\[\\w+=.*?\\](?=\\s*\\[\\w+=|\\s*$)"
      bracketed <- grepl(bracket_pattern, fasta_headers, perl = TRUE)
      matches <- vector("list", length(fasta_headers))
      matches[bracketed] <- gregexpr(bracket_pattern, fasta_headers[bracketed], perl = TRUE)
      matches[!bracketed] <- gregexpr("(\\w+)=(.*?)(?=\\s+\\w+=|$)", fasta_headers[!bracketed], perl = TRUE)

      # vapply(): an empty file has no matches, not an error (B66)
      no_matches <- which(vapply(matches, function(x) any(x == -1), logical(1)))

      if (length(sequences) == length(no_matches)) {
        warning(sprintf("No keys found in '%s'. Make sure the fasta headers are in the right format. Skipping file.", cluster_name))
        next
      }

      if(length(no_matches) > 0){
        warning(sprintf("No keys found in '%s' for seq(s) at index(es): %s. They will be removed.", cluster_name, toString(no_matches)))
        sequences <- sequences[-no_matches]
        fasta_headers <- fasta_headers[-no_matches]
        matches <- matches[-no_matches]
        bracketed <- bracketed[-no_matches]
      }

      # Extract matched strings into one long vector of key-value pairs
      match_start <- unlist(matches, use.names = FALSE)
      match_length <- unlist(lapply(matches, attr, "match.length"), use.names = FALSE)
      pair_id <- rep(seq_along(matches), lengths(matches))
      pairs <- substring(fasta_headers[pair_id], match_start, match_start + match_length - 1)

      # Remove the square brackets around bracketed pairs
      in_brackets <- bracketed[pair_id]
      pairs[in_brackets] <- substring(pairs[in_brackets], 2, nchar(pairs[in_brackets]) - 1)

      # Split at the first '=', then spread the key-value pairs into columns
      equals <- regexpr("=", pairs, fixed = TRUE)
      df_long <- data.frame(
        id = as.character(pair_id),
        key = substring(pairs, 1, equals - 1),
        value = trimws(substring(pairs, equals + 1)),
        stringsAsFactors = FALSE
      )
      df <- widen_attributes(df_long)$data

      # Replace 'NA' with actual NA where applicable
      df[df == "NA"] <- NA

      if (sequence) {
        df$sequence <- sequences
      }

      # Check if 'location' column exists and then extract start and end positions
      if ("location" %in% colnames(df)) {
        ranges <- vapply(df$location, location_span, numeric(2), USE.NAMES = FALSE)
        start_end_df <- data.frame(start = ranges[1, ], end = ranges[2, ])

        # Bind start and end columns to the data frame
        df <- cbind(df, start_end_df)

        # Add strand
        if (any(grepl("complement", df$location))) {
          # Create the 'strand' column based on the presence of the word "complement" in the 'location' column
          df$strand <- ifelse(grepl("complement", df$location), "complement", "forward")
        }
      }

      # Add the cluster name as a new column to the data frame
      df$cluster <- cluster_name

      if(!is.null(keys)){

        missing_keys <- setdiff(keys, colnames(df))

        if(length(missing_keys) > 0) {
          warning("The following keys are not column names in the dataframe: ", paste(missing_keys, collapse = ", "), ".")
        }

        df <- df %>% dplyr::select(dplyr::any_of(keys))
      }

      # Add the data frame for the current file to the list
      all_dfs[[file_path]] <- df
    }, error = function(e) {
      # If an error occurs, issue a warning and skip to the next file
      warning(sprintf("An error occurred while processing file '%s': %s", file_path, e$message))
    })
  }

  # Combine data frames from all files into one
  final_df <- dplyr::bind_rows(all_dfs)

  # Reset row names (optional)
  rownames(final_df) <- NULL

  return(final_df)
}

#' Read the headers and sequences of a FASTA file
#'
#' Reads the file like `Biostrings::readAAStringSet()`: lines starting with
#' ";" are comments and blank lines are skipped, sequence letters are upper
#' cased, and characters that are not one-letter amino acid codes (spaces,
#' digits) are dropped with a warning.
#'
#' @param file_path Path to a FASTA file, which can be gzip compressed.
#' @return A list with the character vectors `headers` and `sequences`.
#' @noRd
read_fasta_records <- function(file_path) {
  lines <- readLines(file_path, warn = FALSE)
  line_number <- seq_along(lines)

  # Drop comment and empty lines
  keep <- !startsWith(lines, ";") & nzchar(lines)
  lines <- lines[keep]
  line_number <- line_number[keep]

  is_header <- startsWith(lines, ">")
  record <- cumsum(is_header)
  if (length(lines) > 0 && !is_header[1]) {
    stop(sprintf('reading FASTA file %s: ">" expected at beginning of line %d',
                 file_path, line_number[1]))
  }

  # Join the sequence lines of each record, keeping records without sequence
  headers <- substring(lines[is_header], 2)
  sequences <- split(lines[!is_header], factor(record[!is_header], levels = seq_along(headers)))
  sequences <- vapply(sequences, paste, character(1), collapse = "", USE.NAMES = FALSE)

  # Keep the one-letter amino acid codes of Biostrings::AA_ALPHABET
  valid <- toupper(gsub("[^A-Za-z*+.-]", "", sequences, useBytes = TRUE))
  invalid <- sum(nchar(sequences, type = "bytes") - nchar(valid, type = "bytes"))
  if (invalid > 0) {
    warning(sprintf("reading FASTA file %s: ignored %d invalid one-letter sequence codes",
                    file_path, invalid))
  }

  list(headers = headers, sequences = valid)
}
