#'Read Protein Sequences from FASTA Files
#'
#'This function reads protein sequences from the specified FASTA file or all
#'FASTA files within a directory. It specifically looks for metadata in the
#'FASTA headers with key-value pairs separated by an equals sign `=`. For
#'example, from the header '>protein1 \[gene=scnD\] \[protein=ScnD\]', it extracts
#''gene' as the key and 'scnD' as its value, and similarly for other key-value
#'pairs.
#'
#'The Biostrings package is required to run this function.
#'
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
#' \dontrun{
#' # Read sequences from a single FASTA file
#' sequences_df <- read_fasta("path/to/single_file.fasta")
#'
#' # Read all sequences from a directory of FASTA files
#' sequences_df <- read_fasta("path/to/directory/", file_extension = "fa")
#'
#' # Read sequences and include the protein sequences in the output
#' sequences_df <- read_fasta("path/to/directory/", sequence = TRUE)
#'}
#'@export
read_fasta <- function(fasta_path, sequence = TRUE, keys = NULL, file_extension = "fasta") {

  # Check if Biostrings package is installed
  if (!requireNamespace("Biostrings", quietly = TRUE)) {
    stop('Biostrings package is not installed. Please install it using BiocManager::install("Biostrings").')
  }

  # Check if dplyr package is installed
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop('dplyr package is not installed. Please install it using install.packages("dplyr").')
  }

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

      # Read the AA sequence set from the file
      sequence_set <- Biostrings::readAAStringSet(file_path)

      # Convert to data frame
      sequence_data <- Biostrings::as.data.frame(sequence_set)

      # Extract headers
      fasta_headers <- row.names(sequence_data)

      # Find matches
      # matches <- gregexpr("\\[.*?\\]", fasta_headers, perl = TRUE)

      # Key-value pairs: "[key=value]" (the value can contain spaces and
      # brackets; it ends at the "]" before the next "[key=" or the end), or
      # without brackets "key=value" running up to the next " key=" or the end
      bracket_pattern <- "\\[\\w+=.*?\\](?=\\s*\\[\\w+=|\\s*$)"
      bracketed <- grepl(bracket_pattern, fasta_headers, perl = TRUE)
      matches <- vector("list", length(fasta_headers))
      matches[bracketed] <- gregexpr(bracket_pattern, fasta_headers[bracketed], perl = TRUE)
      matches[!bracketed] <- gregexpr("(\\w+)=(.*?)(?=\\s+\\w+=|$)", fasta_headers[!bracketed], perl = TRUE)

      no_matches <- which(sapply(matches, function(x) any(x == -1)))

      if (nrow(sequence_data) == length(no_matches)) {
        warning(sprintf("No keys found in '%s'. Make sure the fasta headers are in the right format. Skipping file.", cluster_name))
        next
      }

      if(length(no_matches) > 0){
        warning(sprintf("No keys found in '%s' for seq(s) at index(es): %s. They will be removed.", cluster_name, toString(no_matches)))
        sequence_data <- sequence_data[-no_matches, , drop = FALSE]
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
      df <- widen_attributes(df_long) %>%
        dplyr::select(-"id") # Remove the id column if not needed

      # Replace 'NA' with actual NA where applicable
      df[df == "NA"] <- NA

      if (sequence) {
        df$sequence <- sequence_data$x
      }

      # Check if 'location' column exists and then extract start and end positions
      if ("location" %in% colnames(df)) {
        all_matches <- regmatches(df$location, gregexpr("[0-9]+", df$location))

        # Start and end span all parts of split locations such as
        # join(1..100,200..300). Parts that are not in order (crossing the
        # origin of a circular genome, trans-splicing) keep the first part.
        location_range <- function(x) {
          x <- as.numeric(x)
          if (length(x) < 2) return(c(NA_real_, NA_real_))
          if (is.unsorted(x)) x[1:2] else range(x)
        }
        ranges <- vapply(all_matches, location_range, numeric(2))
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
