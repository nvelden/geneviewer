#' Helper function to validate and clean lists
#'
#' This function takes a list as an argument, checks if it's not NULL and is a list,
#' and then removes any NA or NULL values from it. If the input is not a list,
#' it returns an empty list.
#'
#' @param lst A list that might contain NULL or NA values.
#'
#' @return A list where NULL and NA values have been removed,
#' or an empty list if the input wasn't a list.
cleanList <- function(lst) {
  if (!is.null(lst) && is.list(lst)) {
    return(lst[!sapply(lst, function(x) is.na(x) | is.null(x))])
  } else {
    return(list())
  }
}

#' Extract Data from Symbol or Return Vector
#'
#' This function evaluates the provided expression. If the expression is a symbol,
#' it treats the symbol as a column name and extracts the corresponding column from the provided data frame.
#' If the expression is a call that evaluates to a vector (e.g., `as.character(df$column)`),
#' it returns the evaluated vector.
#'
#' @param data A data frame from which a column might be extracted.
#' @param x An expression that could be a symbol representing a column name or a call that evaluates to a vector.
#'
#' @return A vector extracted from the data frame based on the column name represented by the symbol,
#' or a vector if the expression evaluates to one.
#'
#' @examples
#' df <- data.frame(a = 1:5, b = 6:10)
#' select_column_or_return(df, a)
#' select_column_or_return(df, as.character(df$class))
#'
#' @export
select_column_or_return <- function(data, x) {
  x_nm <- deparse(substitute(x))
  if(x_nm %in% names(data)) return(data[x_nm])
  if(is.symbol(substitute(x)) & !exists(x_nm)) {
    stop("x must be a symbol representing a column, or a vector.")
  }
  if(is.vector(x)) return(x)
  stop("x must be a symbol representing a column, or a vector.")
}

#' Get clusters to update from GCVieweR Object
#'
#' This function retrieves the clusters to be updated from a GCVieweR object based on the provided cluster argument.
#' The function checks if the cluster argument is valid and returns the corresponding clusters.
#'
#' @param GCVieweR An object containing the series of clusters.
#' @param cluster A numeric vector or character vector specifying the clusters to be retrieved.
#' If NULL (default), all clusters are returned.
#'
#' @return A character vector of cluster names.
#'
#'
#' @export
getUpdatedClusters <- function(GCVieweR, cluster) {

  # Get the names of the clusters
  clusters <- names(GCVieweR$x$series)

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

#' Divide Dimension Value
#'
#' This function takes a dimension value (which can be in the format '100px', '100\%', or 100)
#' and divides it by a given divisor. The result is returned in the same format as the input.
#'
#' @param value A character or numeric value representing the dimension.
#' It can be in the format '100px', '100\%', or 100.
#' @param divisor A numeric value by which the dimension value will be divided.
#'
#' @return A character or numeric value representing the divided dimension.
#'
#' @examples
#' divide_dimension_value('100px', 2)  # Returns "50px"
#' divide_dimension_value('100%', 2)  # Returns '50%'
#' divide_dimension_value(100, 2)     # Returns 50
#'
#' @export
divide_dimension_value <- function(value, divisor) {
  # Check if value is a character
  if (is.character(value)) {
    # Extract the numeric part and the unit (e.g., "px" or "%")
    matches <- regmatches(value, regexec("([0-9.]+)([a-zA-Z%]*)", value))[[1]]
    number <- as.numeric(matches[2])
    unit <- matches[3]

    # Divide the numeric part by the divisor
    result <- number / divisor

    # Return the result with the original unit
    return(paste0(result, unit))
  } else if (is.numeric(value)) {
    # If the value is numeric, simply divide by the divisor
    return(value / divisor)
  } else {
    stop("Unsupported value format")
  }
}

#' Identify Non-Coding Regions Between Genes
#'
#' Given a set of gene positions, this function calculates and returns the non-coding
#' regions, potentially with added padding. It also considers overlapping genes and ensures
#' that the starting position is always less than the ending position for each gene.
#'
#' @param genes A data.frame containing the start and end positions of genes.
#' The data.frame must contain columns named 'start' and 'stop'.
#' @param threshold_percentage Numeric. A threshold value given as percentage. Only non-coding
#' regions that are larger than this threshold (relative to the entire genomic region covered)
#' will be returned.
#' @param padding Numeric. A value given as percentage that will be added to the start
#' and subtracted from the end of each non-coding region.
#'
#' @return A data.frame containing the start and stop positions of the identified non-coding regions.
#'
#' @examples
#' genes_data <- data.frame(
#'   start = c(10, 50, 90),
#'   stop = c(40, 80, 120)
#' )
#' get_scale_breaks(genes_data, threshold_percentage = 5, padding = 2)
#'
#' @export
get_scale_breaks <- function(genes, threshold_percentage = 0, padding = 0) {

  # Ensure genes is a data.frame
  if (!is.data.frame(genes)) {
    stop("Input must be a data.frame with 'start' and 'stop' columns.")
  }

  # Check if required columns exist
  if (!all(c("start", "stop") %in% colnames(genes))) {
    stop("The data.frame must contain 'start' and 'stop' columns.")
  }

  # Ensure start is always smaller than stop
  swapped_indices <- genes$start > genes$stop
  genes[swapped_indices, c("start", "stop")] <- genes[swapped_indices, c("stop", "start")]

  # Calculate the entire region's length using min and max
  entire_region_length <- max(genes$stop) - min(genes$start) + 1

  # Calculate the minimum gap length based on threshold_percentage
  min_gap_length <- entire_region_length * threshold_percentage / 100

  # Sort by start
  genes <- genes[order(genes$start), ]

  # Merge overlapping regions
  merged_regions <- list()
  current_region <- genes[1,]

  for(i in 2:nrow(genes)){
    if(genes$start[i] <= current_region$stop){
      current_region$stop <- max(current_region$stop, genes$stop[i])
    } else {
      merged_regions <- append(merged_regions, list(current_region))
      current_region <- genes[i,]
    }
  }

  merged_regions <- append(merged_regions, list(current_region))

  # Extract non-coding regions
  non_coding_regions <- data.frame(start=integer(), stop=integer())

  for(i in 1:(length(merged_regions)-1)){
    non_coding_start <- merged_regions[[i]]$stop + 1
    non_coding_stop <- merged_regions[[i+1]]$start - 1
    gap_length <- non_coding_stop - non_coding_start + 1

    if (gap_length >= min_gap_length) {
      non_coding_regions <- rbind(non_coding_regions, data.frame(start=non_coding_start, stop=non_coding_stop))
    }
  }

  padding_val <- entire_region_length * padding / 100
  non_coding_regions$start <- non_coding_regions$start + padding_val
  non_coding_regions$stop <- non_coding_regions$stop - padding_val

  #convert df to list of lists
  non_coding_regions <-
  apply(non_coding_regions, 1, function(row) {
    as.list(setNames(row, names(row)))
  })

  return(non_coding_regions)
}

#' @export
compute_size <- function(value, length = 2) {
  # Check if value is numeric
  if (is.numeric(value)) {
    return(value / length)
  }

  # Check if value ends with 'px'
  if (grepl("px$", value)) {
    num_val <- as.numeric(sub("px$", "", value))
    return(paste0(num_val / length, "px"))
  }

  # Check if value ends with '%'
  if (grepl("%$", value)) {
    num_val <- as.numeric(sub("%$", "", value))
    return(paste0(num_val / length, "%"))
  }

  stop("Unsupported format")
}

#' @export
get_relative_height <- function(baseHeight, relativeHeight) {
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


