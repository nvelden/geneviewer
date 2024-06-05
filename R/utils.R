#' Helper function to validate and clean lists
#'
#' This function takes a list as an argument, checks if it's not NULL and is a
#' list, and then removes any NA or NULL values from it. If the input is not a
#' list, it returns an empty list.
#'
#' @param lst A list that might contain NULL or NA values.
#'
#' @return A list where NULL and NA values have been removed, or an empty list
#'   if the input wasn't a list.
#' @noRd
cleanList <- function(lst) {
  if (!is.null(lst) && is.list(lst)) {
    return(lst[!sapply(lst, function(x) is.na(x) | is.null(x))])
  } else {
    return(list())
  }
}

#' Extract Data from Symbol or Return Vector
#'
#' This function evaluates the provided expression. If the expression is a
#' symbol, it treats the symbol as a column name and extracts the corresponding
#' column from the provided data frame. If the expression is a call that
#' evaluates to a vector (e.g., `as.character(df$column)`), it returns the
#' evaluated vector.
#'
#' @param data A data frame from which a column might be extracted.
#' @param x An expression that could be a symbol representing a column name or a
#'   call that evaluates to a vector.
#'
#' @return A vector extracted from the data frame based on the column name
#'   represented by the symbol, or a vector if the expression evaluates to one.
#'
#' @examples
#' df <- data.frame(a = 1:5, b = 6:10)
#' select_column_or_return(df, a)
#' select_column_or_return(df, as.character(df$class))
#'
#' @noRd
select_column_or_return <- function(data, x) {
  x_nm <- deparse(substitute(x))
  if(x_nm %in% names(data)) return(data[x_nm])
  if(is.symbol(substitute(x)) & !exists(x_nm)) {
    stop("x must be a symbol representing a column, or a vector.")
  }
  if(is.vector(x)) return(x)
  stop("x must be a symbol representing a column, or a vector.")
}

#' Get clusters to update from geneviewer Object
#'
#' This function retrieves the clusters to be updated from a geneviewer object
#' based on the provided cluster argument. The function checks if the cluster
#' argument is valid and returns the corresponding clusters.
#'
#' @param geneviewer An object containing the series of clusters.
#' @param cluster A numeric vector or character vector specifying the clusters
#'   to be retrieved. If NULL (default), all clusters are returned.
#'
#' @return A character vector of cluster names.
#'
#'
#' @noRd
getUpdatedClusters <- function(geneviewer, cluster) {

  # Get the names of the clusters
  clusters <- names(geneviewer$x$series)

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
#' This function takes a dimension value (which can be in the format '100px',
#' '100\%', or 100) and divides it by a given divisor. The result is returned in
#' the same format as the input.
#'
#' @param value A character or numeric value representing the dimension. It can
#'   be in the format '100px', '100\%', or 100.
#' @param divisor A numeric value by which the dimension value will be divided.
#'
#' @return A character or numeric value representing the divided dimension.
#'
#' @examples
#' divide_dimension_value('100px', 2)  # Returns "50px"
#' divide_dimension_value('100%', 2)  # Returns '50%'
#' divide_dimension_value(100, 2)     # Returns 50
#'
#' @noRd
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
#' Given a set of gene positions, this function calculates and returns the
#' non-coding regions, potentially with added padding. It also considers
#' overlapping genes and ensures that the starting position is always less than
#' the ending position for each gene.
#'
#' @param genes A data.frame containing the start and end positions of genes.
#'   The data.frame must contain columns named 'start' and 'end'.
#' @param threshold_percentage Numeric. A threshold value given as percentage.
#'   Only non-coding regions that are larger than this threshold (relative to
#'   the entire genomic region covered) will be returned.
#' @param padding Numeric. A value given as percentage that will be added to the
#'   start and subtracted from the end of each non-coding region.
#'
#' @return A data.frame containing the start and end positions of the
#'   identified non-coding regions.
#' @importFrom stats setNames
#' @examples
#' genes_data <- data.frame(
#'   start = c(10, 50, 90),
#'   end = c(40, 80, 120)
#' )
#' get_scale_breaks(genes_data, threshold_percentage = 5, padding = 2)
#'
#' @noRd
get_scale_breaks <- function(genes, threshold_percentage = 0, padding = 0) {

  # Ensure genes is a data.frame
  if (!is.data.frame(genes)) {
    stop("Input must be a data.frame with 'start' and 'end' columns.")
  }

  # Check if required columns exist
  if (!all(c("start", "end") %in% colnames(genes))) {
    stop("The data.frame must contain 'start' and 'end' columns.")
  }

  # Ensure start is always smaller than end
  swapped_indices <- genes$start > genes$end
  genes[swapped_indices, c("start", "end")] <- genes[swapped_indices, c("end", "start")]

  # Calculate the entire region's length using min and max
  entire_region_length <- max(genes$end) - min(genes$start) + 1

  # Calculate the minimum gap length based on threshold_percentage
  min_gap_length <- entire_region_length * threshold_percentage / 100

  # Sort by start
  genes <- genes[order(genes$start), ]

  # Merge overlapping regions
  merged_regions <- list()
  current_region <- genes[1,]

  for(i in 2:nrow(genes)){
    if(genes$start[i] <= current_region$end){
      current_region$end <- max(current_region$end, genes$end[i])
    } else {
      merged_regions <- append(merged_regions, list(current_region))
      current_region <- genes[i,]
    }
  }

  merged_regions <- append(merged_regions, list(current_region))

  # Extract non-coding regions
  non_coding_regions <- data.frame(start=integer(), end=integer())

  for(i in 1:(length(merged_regions)-1)){
    non_coding_start <- merged_regions[[i]]$end + 1
    non_coding_end <- merged_regions[[i+1]]$start - 1
    gap_length <- non_coding_end - non_coding_start + 1

    if (gap_length >= min_gap_length) {
      non_coding_regions <- rbind(non_coding_regions, data.frame(start=non_coding_start, end=non_coding_end))
    }
  }

  padding_val <- entire_region_length * padding / 100
  non_coding_regions$start <- non_coding_regions$start + padding_val
  non_coding_regions$end <- non_coding_regions$end - padding_val

  #convert df to list of lists
  non_coding_regions <-
  apply(non_coding_regions, 1, function(row) {
    as.list(stats::setNames(row, names(row)))
  })

  return(non_coding_regions)
}

#' Compute Size
#'
#' Internal function to compute size for GC_grid. This function is used to
#' compute the size based on input value and length.
#'
#' @param value A numeric or character value specifying size.
#' @param length A numeric value specifying length to divide the size by.
#' @return Returns computed size.
#' @noRd
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

  if (is.null(data) || nrow(data) == 0) {
    return(NULL)
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

#' Assign Tracks to Genes Based on Start and End Positions
#'
#' This function assigns tracks to genes in a dataset based on their start
#' and end positions. It ensures that overlapping genes are assigned to
#' different tracks. The function operates by iterating over each gene and
#' finding a suitable track where it can be placed without overlapping.
#'
#' @param data A dataframe containing gene data with at least two columns:
#'   start and end. Each row in the dataframe should represent a gene with
#'   its start and end positions.
#'
#' @return A modified version of the input dataframe that includes an additional
#'   column named 'geneTrack'. This column contains the track number assigned to
#'   each gene, ensuring no overlap in tracks.
#' @examples
#' gene_data <- data.frame(start = c(1, 5, 10), end = c(4, 9, 15))
#' peptides_data <- add_gene_track(gene_data)
#' print(gene_data)
#' @noRd
add_gene_track <- function(data) {
  if (is.null(data) || nrow(data) == 0) {
    return(NULL)
  }
  # Initialize a list to keep track of the start and end positions in each track
  track_ranges <- list()

  # Function to find an appropriate track
  find_track <- function(start, end) {

    if (start > end) {
      temp <- start
      start <- end
      end <- temp
    }

    for (i in seq_along(track_ranges)) {
      overlaps <- any(sapply(track_ranges[[i]], function(range) {
        range_start <- range[1]
        range_end <- range[2]
        (start <= range_end && end >= range_start)
      }))
      if (!overlaps) {
        track_ranges[[i]] <<- c(track_ranges[[i]], list(c(start, end)))
        return(i)
      }
    }
    track_ranges <<- c(track_ranges, list(list(c(start, end))))
    return(length(track_ranges))
  }

  # Assign tracks to each peptide
  data$geneTrack <-
    sapply(1:nrow(data), function(i)
      find_track(data$start[i], data$end[i]))

  return(data)
}

#' Adjust Start and End Positions Based on Group Minimums
#'
#' This function adjusts the start and end positions of genomic elements within
#' each cluster by subtracting the minimum start or end position found in that
#' cluster. This normalization facilitates comparisons and visualizations by
#' aligning the positions relative to the start of each cluster.
#'
#' @param data A dataframe containing genomic elements with at least three columns:
#'   cluster (group identifier), start, and end. Each row represents a genomic
#'   element with its start and end positions within a specific cluster.
#' @importFrom dplyr group_by mutate select
#' @importFrom rlang .data
#' @noRd
adjust_to_range <- function(data, cluster = "cluster"){

  if(!is.null(cluster) && cluster %in% colnames(data)){
  data <- data %>%
    dplyr::group_by(.data[[cluster]]) %>%
    dplyr::mutate(
      min_start_end = min(.data$start, .data$end),
      start = .data$start - .data$min_start_end + 1,
      end = .data$end - .data$min_start_end +1
    ) %>%
    dplyr::select(-.data$min_start_end)
  } else {
    data <- data %>%
      dplyr::mutate(
        min_start_end = min(.data$start, .data$end),
        start = .data$start - .data$min_start_end + 1,
        end = .data$end - .data$min_start_end + 1
      ) %>%
      dplyr::select(-.data$min_start_end)
  }

  return(data)
}
