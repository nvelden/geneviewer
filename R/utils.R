#' The clusters an argument selects
#'
#' @param clusters Character, the names of the chart's clusters, in order
#'   (`chart_fields()$clusters`).
#' @param cluster A numeric vector or character vector specifying the clusters
#'   to be retrieved. If NULL (default), all clusters are returned.
#'
#' @return A character vector of cluster names; NULL, with a warning, when
#'   `cluster` names clusters the chart does not have.
#' @noRd
getUpdatedClusters <- function(clusters, cluster) {

  # If cluster is NULL, update all clusters
  if (is.null(cluster)) {
    cluster <- clusters
  } else if (is.numeric(cluster)) {
    # If cluster is numeric, map the numbers to cluster names
    if (any(cluster > length(clusters) | cluster < 1)) {
      warning("Some cluster numbers provided are out of range. Please check the cluster numbers.")
      return(NULL)
    }
    cluster <- clusters[cluster]
  } else if (!all(cluster %in% clusters)) {
    warning("Some cluster names provided are not valid. Please check the cluster names.")
    return(NULL)
  }

  return(cluster)
}

#' Check the clusters an argument selects
#'
#' Warns, as the GC_ functions did, when `cluster` names clusters the chart
#' does not have; the library leaves such an entry out.
#' @noRd
check_clusters <- function(GC_chart, cluster) {
  if (!is.null(cluster)) getUpdatedClusters(chart_fields(GC_chart)$clusters, cluster)
  invisible()
}

#' The base pairs of a padding given as a string, e.g. "500bp"
#' @noRd
break_padding_bp <- function(padding) {
  if (!grepl("^\\s*[0-9]*\\.?[0-9]+\\s*bp\\s*$", padding)) {
    stop("scale_break_padding must be a percentage, e.g. 1, or base pairs, e.g. \"500bp\"")
  }
  as.numeric(sub("\\s*bp\\s*$", "", padding))
}

#' Get Relative Height
#'
#' Internal function to calculate relative height for GC_grid. This function is
#' used to adjust the height of elements based on the base height.
#'
#' @param baseHeight A numeric or character value specifying the base height.
#' @param relativeHeight A numeric or character value specifying the height
#'   relative to the base height.
#' @return Returns the calculated relative height.
#' @noRd
get_relative_height <- function(baseHeight, relativeHeight) {
  # Check if both baseHeight and relativeHeight are in percentage
  if(is.character(baseHeight) && grepl("%$", baseHeight) &&
     is.character(relativeHeight) && grepl("%$", relativeHeight)) {
    # Return relativeHeight as is
    return(relativeHeight)
  }
  # Convert baseHeight to numeric if it's in the format "400px"
  if(is.character(baseHeight) && grepl("px$", baseHeight)) {
    baseHeight <- as.numeric(sub("px", "", baseHeight))
  }

  # Check if relativeHeight is a percentage
  if(is.character(relativeHeight) && grepl("%$", relativeHeight)) {
    # Convert percentage to a decimal and calculate the new height
    percentage <- as.numeric(sub("%", "", relativeHeight)) / 100
    return(baseHeight * percentage)
  } else {
    # If relativeHeight is numeric or a string like "400px", treat it as numeric
    if(is.character(relativeHeight) && grepl("px$", relativeHeight)) {
      relativeHeight <- as.numeric(sub("px", "", relativeHeight))
    }
    return(as.numeric(relativeHeight))
  }
}


#' Adjust strand orientation based on start and end positions
#'
#' This function adjusts the strand orientation of genomic data based on
#' start and end positions. It ensures that if `strand` equals 1, `start`
#' should be less than `end`, and if `strand` equals -1, `end` should be
#' less than `start`. If this condition is not met, it swaps the values
#' of `start` and `end`. The function also validates the `strand` values.
#'
#' @param data A dataframe containing genomic data with at least two columns:
#'   `start` and `end`. The dataframe may also contain a `strand` column.
#' @param strand Optional parameter to specify the column name containing
#'   strand information. If NULL, the strand is determined based on start
#'   and end positions.
#'
#' @return A modified version of the input dataframe with adjusted strand
#'   orientations and potentially swapped start and end values.
#' @examples
#' example_data <- data.frame(start = c(3, 2, 5), end = c(2, 4, 6), strand = c(1, -1, 1))
#' adjusted_data <- add_strand(example_data)
#' print(adjusted_data)
#' @noRd
add_strand <- function(data, strand = NULL){

  if (is.null(data)) {
    return(NULL)
  }
  # Data without rows keeps its columns, with an empty strand (B75)
  if (nrow(data) == 0) {
    data$strand <- character(0)
    return(data)
  }

  colnames_data <- colnames(data)

  if(is.null(strand) && "strand" %in% colnames_data){
    strand <- "strand"
  }

  if (!("start" %in% colnames_data)) stop("start column not found in data")
  if (!("end" %in% colnames_data)) stop("end column not found in data")

  if (is.null(strand)){
    data$strand <- ifelse(data$start < data$end, "forward", "reverse")
  } else {
    if (!(strand %in% colnames_data)) stop("strand column not found in data")

    forward <- c(1, "forward", "+", "sense")
    reverse <- c(-1, 0, "reverse", "-", "antisense", "complement")

    if (!all(data[[strand]] %in% c(forward, reverse))) {
      stop("Invalid strand values found. Valid values are 1/forward/+/sense or -1/0/reverse/-/antisense/complement.")
    }

    # Replace values in the strand column
    data$strand <- ifelse(data[[strand]] %in% forward, "forward",
                          ifelse(data[[strand]] %in% reverse, "reverse", data[[strand]]))

    # Swap start and end if conditions are not met
    swap_needed <- (data$strand == "forward" & data$start > data$end) | (data$strand == "reverse" & data$start < data$end)
    data[swap_needed, c("start", "end")] <- data[swap_needed, c("end", "start")]


  }

  return(data)

}

#' Update Start and End Columns Based on Merged Data
#'
#' This function merges two data frames by a specified column (default is
#' "rowID") and updates the columns of the original data that the updated data
#' also has, such as 'start' and 'end', where the updated data has a
#' non-missing value. Columns only in the updated data are added.
#'
#' @param data A data frame containing the original data.
#' @param updated_data A data frame containing the updated data with the same
#'   'rowID' column.
#' @param by_col A character string specifying the column name to merge the data
#'   frames by. Default is "rowID".
#' @return A data frame with the updated columns.
#' @noRd
update_cluster_data <- function(data, updated_data, by_col = "rowID") {
  # Check if the 'rowID' column exists in both data frames
  if (!(by_col %in% names(data))) {
    stop(paste("The column", by_col, "does not exist in the 'data' data frame."))
  }

  if (!(by_col %in% names(updated_data))) {
    stop(paste("The column", by_col, "does not exist in the 'updated_data' data frame."))
  }

  # Merge the data frames
  result <- merge(data, updated_data, by = by_col, all.x = TRUE, suffixes = c("", "_new"))

  # Update the columns of both, e.g. 'start', 'end' and the 'original_start'
  # a cluster normalised before added, with the new values where available
  for (column in sub("_new$", "", grep("_new$", names(result), value = TRUE))) {
    new <- result[[paste0(column, "_new")]]
    available <- !is.na(new)
    result[[column]][available] <- new[available]
  }

  # Remove the columns with the "_new" suffix
  result <- result[, !grepl("_new$", names(result))]

  return(result)
}


#' Value for the i-th selected cluster
#'
#' Options given as vectors are recycled over the selected clusters.
#'
#' @param x A vector or list, or NULL.
#' @param i Integer, the index of the cluster.
#' @return The element of `x` for cluster `i` (a length-one vector or list), or
#'   NULL when `x` is NULL.
#' @noRd
recycle_at <- function(x, i) {
  x[(i - 1) %% length(x) + 1]
}

#' Check an align argument
#'
#' @param align NULL, or "left", "center" or "right"; one per cluster when
#'   `per_cluster` is TRUE.
#' @param per_cluster Logical, whether `align` may have one value per cluster.
#' @return `align`, invisibly; an error when it is not valid.
#' @noRd
check_align <- function(align, per_cluster = FALSE) {
  if (is.null(align)) {
    return(invisible(align))
  }
  valid <- is.character(align) && length(align) >= 1 &&
    all(align %in% c("left", "center", "right"))
  if (!valid || (!per_cluster && length(align) != 1)) {
    stop('align must be "left", "center" or "right"')
  }
  invisible(align)
}

#' Check a spacing argument
#'
#' @param spacing NULL, or a number: pixels between a title and its subtitle;
#'   one per cluster when `per_cluster` is TRUE.
#' @param per_cluster Logical, whether `spacing` may have one value per cluster.
#' @return `spacing`, invisibly; an error when it is not valid.
#' @noRd
check_spacing <- function(spacing, per_cluster = FALSE) {
  if (is.null(spacing)) {
    return(invisible(spacing))
  }
  valid <- is.numeric(spacing) && length(spacing) >= 1 && all(is.finite(spacing))
  if (!valid || (!per_cluster && length(spacing) != 1)) {
    stop(if (per_cluster) "spacing must be numbers" else "spacing must be a single number")
  }
  invisible(spacing)
}


#' Files of a directory to read
#'
#' @param path Character, the directory.
#' @param pattern Regular expression the file names match.
#' @param type Character, the file type named in the error, e.g. "bed".
#' @param call The call named in the error: of the reader called by the user.
#' @return The paths of the files; an error when there are none.
#' @noRd
dir_files <- function(path, pattern, type, call = sys.call(-1)) {
  files <- list.files(path, pattern = pattern, full.names = TRUE)
  if (length(files) == 0) {
    stop(simpleError(paste0("No .", type, " files found in the specified directory."), call))
  }
  files
}

#' Start and end of a GenBank feature location
#'
#' The span runs over all parts of a split location, from the lowest to the
#' highest position, e.g. join(complement(5441..5600),complement(5095..5373))
#' gives 5095 and 5600. Parts inside join(complement(..),complement(..)) are
#' listed from right to left, all other parts from left to right. Parts out of
#' that order (crossing the origin of a circular genome, trans-splicing) keep
#' the first part.
#'
#' @param location Character, a location such as "complement(join(1..100,200..300))".
#' @return Numeric vector of the start and end, NA when there is no position.
#' @noRd
location_span <- function(location) {
  if (is.na(location)) return(c(NA_real_, NA_real_))
  parts <- regmatches(location, gregexpr("[<>]?\\d+\\.\\.[<>]?\\d+", location))[[1]]
  if (length(parts) == 0) {
    # A single base, such as 123 or complement(123)
    base <- regmatches(location, gregexpr("\\d+", location))[[1]]
    if (length(base) == 1) return(rep(as.numeric(base), 2))
    return(c(NA_real_, NA_real_))
  }
  pos <- matrix(as.numeric(gsub("[<>]", "", unlist(strsplit(parts, "..", fixed = TRUE)))),
                ncol = 2, byrow = TRUE)
  starts <- pos[, 1]
  if (grepl("^\\s*(join|order)\\(\\s*complement", location)) starts <- rev(starts)
  if (is.unsorted(starts)) return(pos[1, ])
  range(pos)
}

#' Read a file, or all files of a directory into one data frame
#'
#' @param path Character, a file or a directory.
#' @param pattern Regular expression of the file extension, removed from the
#'   file names for the `filename` column.
#' @param type Character, the file type named in the error, e.g. "bed".
#' @param read_file Function reading one file into a data frame.
#' @param call The call named in errors: of the reader called by the user.
#' @return The data frame of the file, or of all files of the directory with
#'   the name of each file in a `filename` column.
#' @noRd
read_tables <- function(path, pattern, type, read_file, call = sys.call(-1)) {
  if (dir.exists(path)) {
    data_list <- list()
    for (file in dir_files(path, pattern, type, call)) {
      data <- read_file(file)
      filename <- sub(pattern, "", basename(file))
      data$filename <- filename
      data_list[[filename]] <- data
    }
    # Combine data frames from all files into one
    do.call(dplyr::bind_rows, data_list)
  } else if (file.exists(path)) {
    read_file(path)
  } else {
    stop(simpleError("The specified path does not exist.", call))
  }
}

#' The score column of a GFF or BED file as numbers
#'
#' `"."` and empty values, no score, become NA. Other values that are not
#' numbers also become NA, with a warning.
#' @noRd
score_to_numeric <- function(x) {
  x <- trimws(as.character(x))
  x[x %in% c(".", "")] <- NA
  numbers <- suppressWarnings(as.numeric(x))
  lost <- !is.na(x) & is.na(numbers)
  if (any(lost)) {
    warning("Scores that are not numbers were set to NA: ",
            paste(utils::head(unique(x[lost]), 5), collapse = ", "), call. = FALSE)
  }
  numbers
}
