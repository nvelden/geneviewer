#' Read Data from GeneBank Files
#'
#' This function reads data from a single GeneBank file or directorty with
#' GenBank files. It allows selective extraction of information by specifying
#' sections and features.
#'
#' @param path A string representing the file path to the target GeneBank (.gbk)
#'   file or directory.
#' @param sections An optional vector of strings representing the names of
#'   specific sections within the GeneBank file to extract (e.g., "LOCUS",
#'   "DEFINITION", "ACCESSION", "VERSION"). If `NULL` (the default), the
#'   function extracts all available sections.
#' @param features An optional vector of strings indicating specific feature
#'   types to extract from the FEATURES section of the GeneBank file (e.g.,
#'   "CDS", "gene", "mRNA"). If `NULL` (the default), the function extracts all
#'   feature types present in the FEATURES section.
#' @param origin A boolean flag; when set to `TRUE` (the default), the origin
#'   sequence data is included in the output.
#' @return A list containing the contents of the specified sections and features
#'   of the GeneBank file. Each section and feature is returned as a separate
#'   list element.
#'
#' @examples
#' \dontrun{
#' # Read all data from a GeneBank file
#' gbk_data <- read_gbk("path/to/genebank_file.gbk")
#'
#' # Read all data from a directory of GeneBank files
#' gbk_data <- read_gbk("path/to/genebank/directory")
#'
#' # Read only specific sections from a GeneBank file
#' gbk_data <- read_gbk(
#'   "path/to/genebank_file.gbk",
#'   sections = c("LOCUS", "DEFINITION")
#' )
#'
#' # Read specific features from the FEATURES section of a GeneBank file
#' gbk_data <- read_gbk("path/to/genebank_file.gbk", features = c("gene", "CDS"))
#'
#' # Read data without the origin sequence
#' gbk_data <- read_gbk("path/to/genebank_file.gbk", origin = FALSE)
#' }
#' @export
read_gbk <- function(path, sections = NULL, features = NULL, origin = TRUE) {

  results <- list()

  if (dir.exists(path)) {
    # It's a directory
    files <- list.files(path, pattern = "\\.gbk$", full.names = TRUE)

    # Check if there are any .gbk files in the directory
    if (length(files) == 0) {
      stop("No .gbk files found in the specified directory.")
    }

    # Process each .gbk file in the directory
    basename <- sapply(files, function(x) sub("\\.gbk$", "", basename(x)))
    results <- setNames(lapply(files, function(file) process_gbk(file, sections, features, origin)),
                        basename)

  } else if (file.exists(path)) {
    basename <- sub("\\.gbk$", "", basename(path))
    results[[basename]] <- process_gbk(path, sections, features, origin)
  } else {
    stop("The specified path does not exist.")
  }

  return(results)
}

#' Convert GeneBank Features to Data Frame
#'
#' This function processes a list of GeneBank features (loaded by read_gbk())
#' and converts selected features into a data frame. It supports processing
#' multiple gene clusters.
#'
#' @param gbk_list A list of lists where each sub-list contains GeneBank
#'   features for a specific gene cluster. Each sub-list is expected to have a
#'   named list of features, with each feature being a character vector.
#' @param feature A string specifying the feature type to extract from each gene
#'   cluster's FEATURE list (e.g., "CDS" or "gene"). Defaults to "CDS".
#' @param keys An optional vector of strings representing specific keys within
#'   the feature to retain in the final data frame. If `NULL` (the default), all
#'   keys within the specified feature are included.
#' @param process_region A boolean flag; when set to `TRUE` (the default),
#'   special processing is performed on the 'region' key (if present) to extract
#'   'strand', 'start', and 'end' information.
#' @return A data frame where each row corresponds to a feature from the input
#'   list. The data frame includes a 'cluster' column indicating the source gbk
#'   file.
#'
#' @examples
#' \dontrun{
#' gbk <- read_gbk("path/to/genebank_file.gbk")
#' df <- gbk_features_to_df(gbk)
#'
#' # To extract only specific keys within the "CDS" feature
#' df <- gbk_features_to_df(gbk, feature = "CDS", keys = c("gene", "region"))
#'
#' # To disable special processing of the 'region' key
#' df <- gbk_features_to_df(gbk, process_region = FALSE)
#' }
#' @export
gbk_features_to_df <- function(gbk_list, feature = "CDS", keys = NULL, process_region = TRUE) {

  # Initialize an empty list to store DataFrames
  df_list <- list()

  # Iterate over each cluster (file) in the gbk_list
  for (cluster_name in names(gbk_list)) {
    list_item <- gbk_list[[cluster_name]]

    if (!feature %in% names(list_item[["FEATURES"]])) {
      warning(paste("Feature", feature, "not found in cluster", cluster_name))
      next
    }

    feature_list <- list_item[["FEATURES"]][[feature]]

    df <- bind_rows(lapply(feature_list, function(x) {
      # If keys are defined, subset x to include only the specified keys
      if (!is.null(keys)) {
        x <- x[keys]
      }
      # Convert named character vectors to a dataframe
      data.frame(t(x), stringsAsFactors = FALSE)
    }))

    # Mark rows that are entirely NA
    all_na_rows <- apply(is.na(df), 1, all)

    # Add the cluster name column
    df$cluster <- cluster_name

    # Remove NA rows
    df <- df[!all_na_rows, ]

    # Remove columns that are entirely NA
    df <- df[, colSums(is.na(df)) < nrow(df)]

    if (process_region && "region" %in% names(df)) {
      df$strand <- ifelse(is.na(df$region), NA,
                          ifelse(grepl("\\(", df$region),
                                 trimws(sub("\\(.*", "", df$region)),
                                 "forward"))

      df$start <- as.numeric(sub("[^0-9]*(\\d+)\\.\\..*$", "\\1", df$region))
      df$end <- as.numeric(gsub(".*\\.\\.[^0-9]*([0-9]+).*", "\\1", df$region))
    }

    # Append the df to df_list
    df_list[[cluster_name]] <- df
  }

  # Combine all dataframes into one
  combined_df <- bind_rows(df_list)
  return(combined_df)
}

#' @noRd
gbk_get_feature_keys <- function(lines){
  pattern <- "^ {5}[a-zA-Z]\\w*"
  matched_lines <- grep(pattern, lines, value = TRUE)
  keys <- sub("^ {5}(\\S+).*", "\\1", matched_lines)
  return(unique(keys))
}

#' @noRd
gbk_get_sections <- function(lines, keys) {

  # Find all section starts
  all_section_starts <- which(grepl("^ {0,2}([^ ]+)(?=\\s|$)", lines, perl = TRUE))

  # Initialize a list to store the content of each section
  sections <- list()

  # Calculate the end of each section and extract content
  for (i in 1:length(keys)) {
    # Find the start of the current section
    section_starts <- which(grepl(paste0("^\\s{0,2}", keys[i]), lines, perl = TRUE))

    # If the section is found, process it
    if (length(section_starts) > 0) {
      # Find the next section start after the current section start, or end of lines
      next_starts <- all_section_starts[all_section_starts > section_starts[1]]
      section_end <- if (length(next_starts) > 0) next_starts[1] - 1 else length(lines)

      # Extract the lines of the current section
      section_lines <- lines[section_starts[1]:section_end]

      if(keys[i] == "FEATURES"){
        section_text <- section_lines
      } else {
        # Combine lines into a single string
        section_text <- paste(section_lines, collapse = "\n")
        # Clean and process the section text
        section_text <- gsub(paste0("^\\s{0,2}", keys[i], "\\s+"), "", section_text)
        section_text <- gsub("\\s+", " ", section_text)
        section_text <- trimws(section_text)
      }

      # Store the section text in the list
      sections[[keys[i]]] <- section_text
    }
  }

  if(!is.null(sections[["ORIGIN"]])){
    sections[["ORIGIN"]] <- gsub("[^a-zA-Z]", "", sections[["ORIGIN"]])
  }

  # Return the list of sections
  return(sections)
}

#' @noRd
gbk_process_features <- function(lines, key){

  # Extract keys
  key_pattern <- "(?<=/)([^/=]+)(?==)"
  key_matches <- gregexpr(key_pattern, lines, perl=TRUE)
  keys <- unlist(regmatches(lines, key_matches))

  # Extract values
  value_pattern <- "(?<=\\=)[^/]+(?=\\/|$)"
  value_matches <- gregexpr(value_pattern, lines, perl=TRUE)
  values <- unlist(regmatches(lines, value_matches))
  values <- gsub("^[^a-zA-Z0-9]+|[^a-zA-Z0-9]+$", "", values)

  # Combine keys and values into a named vector
  key_values <- setNames(values, keys)

  # Extract region based on the key
  # Fix the regex pattern and extraction process
  region_pattern <- sprintf(".*?%s\\s+(.*?)\\s*\\/.*", key)
  region <- sub(region_pattern, "\\1", lines)

  # If the pattern isn't found, sub returns the original string, so check for that
  if (region == lines) {
    region <- NA
  }

  key_values["region"] <- region

  # remove spaces from translation
  if(!is.null(key_values["translation"])){
    key_values["translation"] <- gsub("[^a-zA-Z]", "", key_values["translation"])
  }


  return(key_values)
}

#' @noRd
gbk_get_features <- function(lines, key) {

  # Find the starts of the sections
  section_starts <- which(grepl(paste0("^\\s*", key), lines))

  # If the section is not found, return NULL
  if (length(section_starts) == 0) {
    return(NULL)
  }

  # Find all potential section start lines
  all_section_starts <- which(grepl("^ {5}[a-zA-Z]", lines))

  # Initialize a list to store the content of each section
  sections_content <- vector("list", length(section_starts))

  # Calculate the end of each section
  for (i in 1:length(section_starts)) {
    # Find the next section start after the current section start
    next_starts <- all_section_starts[all_section_starts > section_starts[i]]
    if (length(next_starts) > 0) {
      section_end <- next_starts[1] - 1
    } else {
      section_end <- length(lines)
    }

    # Extract the lines of the current section
    section_lines <- lines[section_starts[i]:section_end]

    # Combine lines into a single string
    section_text <- paste(section_lines, collapse = "\n")

    # Store the section text in the list
    sections_content[[i]] <- section_text
  }

  # Process the extracted sections
  sections_content <- lapply(sections_content, function(section) {

    section <- gsub("\\s+", " ", section)
    section <- trimws(section)
    section <- gbk_process_features(section, key)
  })

  # Return the list of sections
  return(sections_content)
}

#' @noRd
process_gbk <- function(file, sections = NULL, features = NULL, origin = TRUE) {

  # Check if the file exists
  if (!file.exists(file)) {
    stop("The specified file does not exist.")
  }

  lines <- readLines(file)

  # Get actual section keys from the file
  actual_section_keys <- gbk_get_section_keys(lines)

  # User-defined section keys or all section keys from the file
  section_keys <- if (!is.null(sections)) sections else actual_section_keys

  # Remove "ORIGIN" from section_keys if origin is FALSE
  if (!origin) {
    section_keys <- setdiff(section_keys, "ORIGIN")
  }

  # Warn if any user-defined section key is not found in the actual section keys
  missing_sections <- setdiff(section_keys, actual_section_keys)
  if (length(missing_sections) > 0) {
    warning("The following sections were not found in the file: ", paste(missing_sections, collapse = ", "))
  }

  # Get sections
  if(!is.null(section_keys) && length(section_keys) > 0){
  section_values <- gbk_get_sections(lines, section_keys)
  } else {
  section_values <- list()
  }
  # Get actual feature keys from the FEATURES section of the file
  actual_feature_keys <- gbk_get_feature_keys(section_values[["FEATURES"]])

  # User-defined feature keys or all feature keys from the FEATURES section
  feature_keys <- if (!is.null(features)) features else actual_feature_keys

  # Warn if any user-defined feature key is not found in the actual feature keys
  missing_features <- setdiff(feature_keys, actual_feature_keys)
  if (length(missing_features) > 0) {
    warning("The following features were not found in the FEATURES section: ", paste(missing_features, collapse = ", "))
  }

  # Get Features
  feature_section <- section_values[["FEATURES"]]
  feature_values <- lapply(feature_keys, function(name) gbk_get_features(feature_section, name))
  names(feature_values) <- feature_keys
  section_values[["FEATURES"]] <- feature_values

  return(section_values)
}
