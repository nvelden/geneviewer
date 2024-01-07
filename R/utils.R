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

#' Get clusters to update from GCVieweR Object
#'
#' This function retrieves the clusters to be updated from a GCVieweR object
#' based on the provided cluster argument. The function checks if the cluster
#' argument is valid and returns the corresponding clusters.
#'
#' @param GCVieweR An object containing the series of clusters.
#' @param cluster A numeric vector or character vector specifying the clusters
#'   to be retrieved. If NULL (default), all clusters are returned.
#'
#' @return A character vector of cluster names.
#'
#'
#' @noRd
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
  if (!("start" %in% colnames_data)) stop("start column not found in data")
  if (!("end" %in% colnames_data)) stop("end column not found in data")

  if(is.null(strand)){
    data$strand <- ifelse(data$start < data$end, 1, -1)
  } else {
    if (!(strand %in% colnames_data)) stop("strand column not found in data")

    forward <- c(1, "forward")
    reverse <- c(-1, 0, "reverse")

    if (!all(data[[strand]] %in% c(forward, reverse))) {
      stop("Invalid strand values found. Valid values are 1/forward or -1/0/reverse.")
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

#' Adjust Minimum X Coordinate Based on Relative Position
#'
#' This function adjusts the minimum x coordinate (`xmin`) based on a specified
#' point and its relative position between `xmin` and `xmax`. It ensures that the
#' adjusted `xmin` does not exceed the original `xmin` and handles cases where
#' the relative position is outside the 0 to 1 range or the point is outside the
#' `xmin` to `xmax` range.
#'
#' @param xmin The original minimum x coordinate.
#' @param xmax The maximum x coordinate.
#' @param point A point between `xmin` and `xmax`.
#' @param relative_position A number between 0 and 1 indicating the relative position
#'   of `point` between `xmin` and `xmax`.
#' @param reverse A logical value indicating whether the cluster is reversed or not
#'
#' @return The adjusted minimum x coordinate which is always less than or equal to
#'   the original `xmin`.
#' @noRd
adjusted_xmin <- function(xmin, xmax, point, relative_position, reverse = FALSE) {

  if(reverse){
    relative_position <- 1 - relative_position
  }

  if (relative_position <= 0 || relative_position >= 1) {
    return(xmin)
  }

  if (point > xmax || point < xmin) {
    return(xmin)
  }

  adjusted_xmin <- point - relative_position * (xmax - point) / (1 - relative_position)
  return(round(min(adjusted_xmin, xmin)))
}

#' Adjust Maximum X Coordinate Based on Relative Position
#'
#' This function adjusts the maximum x coordinate (`xmax`) based on a specified
#' point and its relative position between `xmin` and `xmax`. It ensures that the
#' adjusted `xmax` does not fall below the original `xmax` and handles cases where
#' the relative position is outside the 0 to 1 range or the point is outside the
#' `xmin` to `xmax` range.
#'
#' @param xmin The original minimum x coordinate.
#' @param xmax The maximum x coordinate.
#' @param point A point between `xmin` and `xmax`.
#' @param relative_position A number between 0 and 1 indicating the relative position
#'   of `point` between `xmin` and `xmax`.
#' @param reverse A logical value indicating whether the cluster is reversed or not
#' @return The adjusted maximum x coordinate which is always greater than or equal to
#'   the original `xmax`.
#' @noRd
adjusted_xmax <- function(xmin, xmax, point, relative_position, reverse = FALSE) {
  if(reverse){
    relative_position <- 1 - relative_position
  }

  if (relative_position <= 0 || relative_position >= 1) {
    return(xmax)
  }

  if (point > xmax || point < xmin) {
    return(xmax)
  }

  adjusted_xmax <- point + (1 - relative_position) * (point - xmin) / relative_position
  return(round(max(adjusted_xmax, xmax)))
}

#' Calculate Position and Offset within a Cluster
#'
#' This function determines the position and offset of a gene within a cluster based on
#' the specified alignment (left, right, or center). It also accounts for the orientation
#' of the cluster (normal or reversed) and adjusts the position accordingly. The offset
#' is calculated as the difference between the gene's position and the cluster's start point.
#'
#' @param align A string indicating the align of the gene to consider for position calculation.
#'   Valid values are "left", "right", or "center". The function will default to "left"
#'   if an invalid align is provided.
#' @param start The start coordinate of the gene.
#' @param end The end coordinate of the gene.
#' @param cluster_start The start coordinate of the cluster.
#' @param reversed A logical value indicating whether the cluster is in a reversed orientation.
#'
#' @return A list containing two elements: `position`, which is the calculated position of the
#'   gene within the cluster, and `offset`, which is the difference between the gene's position
#'   and the cluster's start point.
#' @noRd
get_position_and_offset <- function(align, start, end, cluster_start, reversed) {
  if ((align == "left" && !reversed) || (align == "right" && reversed)) {
    position <- start
  } else if ((align == "right" && !reversed) || (align == "left" && reversed)) {
    position <- end
  } else if (align == "center") {
    position <- (start + end) / 2
  } else {
    warning("Alignment should be 'left', 'right', or 'center'. Defaulting to 'left'.")
    position <- start
  }

  offset <- position - cluster_start

  list(position = position, offset = offset)
}

#' Calculate Relative Positions of Genes within Clusters
#'
#' This function calculates the relative positions of genes within specified
#' clusters. It considers the gene's position (start, end, or center) within
#' the cluster and computes its relative position with respect to the cluster's
#' start and end points. It also includes checks for gene uniqueness within
#' clusters.
#'
#' @param data A dataframe containing gene data, which must include cluster,
#'   group, start, and end columns.
#' @param cluster The name of the column in `data` representing cluster identifiers.
#' @param group The name of the column in `data` representing group identifiers.
#' @param gene The specific gene name to filter on.
#' @param align A string indicating the align of the gene to consider for
#'   calculating relative position. Valid values are "left", "right", or "center".
#'   Defaults to "left".
#' @param reversed_clusters A vector of cluster names where the clusters are considered
#'   to be in the reversed orientation.
#'
#' @return A dataframe with the original data plus two additional columns: 'position',
#'   indicating the absolute position of the gene, and 'relative_position', indicating
#'   the gene's position relative to the cluster's boundaries.
#' @importFrom dplyr group_by summarise ungroup select left_join filter sym n
#' @importFrom rlang .data
#' @noRd
calculate_relative_positions <- function(data, cluster, group, gene, align = "left", reversed_clusters) {

  gene_name <- gene
  cluster_column <- cluster

  # Dynamically aggregate min and max
  cluster_min_max <- data %>%
    dplyr::group_by(!!sym(cluster)) %>%
    dplyr::summarise(cluster_start = min(c(.data$start, .data$end)), cluster_end = max(c(.data$start, .data$end))) %>%
    dplyr::ungroup()

  data <- data %>%
    dplyr::select(!! dplyr::sym(cluster_column), !! dplyr::sym(group), .data$start, .data$end) %>%
    dplyr::left_join(cluster_min_max, by = cluster_column) %>%
    dplyr::filter(.data[[group]] == gene_name)

  # Check if gene is not unique in any cluster
  gene_counts <- data %>%
    dplyr::group_by(!!sym(cluster_column)) %>%
    dplyr::summarise(count = dplyr::n()) %>%
    dplyr::ungroup()

  if (any(gene_counts$count > 1)) {
    warning(paste("The selected gene", gene_name, "appears multiple times in some clusters.",
                  "Please make sure to use a unique gene id."))
    return(NULL)
  }

  # Add if cluster is reversed or not
  data <- data %>%
    dplyr::mutate(
      reversed = !!sym(cluster_column) %in% reversed_clusters
    )

  # Filtering for the specified gene in the group column
  data <- data %>%
    rowwise() %>%
    dplyr::mutate(
      position = get_position_and_offset(
        align,
        .data$start,
        .data$end,
        .data$cluster_start,
        .data$reversed)$position,
      relative_position = get_position_and_offset(
        align,
        .data$start,
        .data$end,
        .data$cluster_start,
        .data$reversed)$offset / (.data$cluster_end - .data$cluster_start)
    )

  return(data)
}

#' Adjust Gene Range Based on Mean Relative Position within a Cluster
#'
#' This function adjusts the start and end range of genes in a dataset based on their
#' mean relative position within a specified cluster. It first calculates the relative
#' positions of genes using `calculate_relative_positions`, then adjusts their start
#' and end positions using `adjusted_xmin` and `adjusted_xmax` functions.
#'
#' @param data A dataframe containing gene data with at least two columns: start
#'   and end. Each row should represent a gene.
#' @param cluster The name of the column in `data` representing cluster identifiers.
#' @param group The name of the column in `data` representing group identifiers.
#' @param gene The specific gene name to filter on.
#' @param align A string indicating the align of the gene to consider for calculating
#'   relative position. Valid values are "left", "right", or "center". Defaults to "left".
#' @param reversed_clusters A vector of cluster names where the clusters are considered
#'   to be in the reversed orientation.
#' @return A modified version of the input dataframe that includes two additional
#'   columns: 'new_start' and 'new_end', representing the adjusted start and end positions
#'   of each gene.
#' @importFrom dplyr rowwise mutate
#' @importFrom rlang .data
#' @noRd
adjust_range <- function(data, cluster, group, gene, align = "left", reversed_clusters = NULL){

  if (!(cluster %in% base::colnames(data))) {
    warning(paste("Selected gene coud not be aligned. Column", cluster, "does not exist in the data."))
    return(NULL)
  }

  if (!(group %in% base::colnames(data))) {
    warning(paste("Selected gene coud not be aligned. Column", group, "does not exist in the data."))
    return(NULL)
  }

  if (!any(data[[group]] == gene)) {
    warning(paste("Selected gene coud not be aligned. Gene", gene, "not found in the", group, "column."))
    return(NULL)
  }

  data <-
    calculate_relative_positions(
      data,
      cluster = cluster,
      group = group ,
      gene = gene,
      align = align,
      reversed_clusters = reversed_clusters
    )

  if(is.null(data)){
    return(data)
  }

  new_relative_position <- mean(data$relative_position)

  data <- data %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      start = adjusted_xmin(.data$cluster_start, .data$cluster_end, .data$position, new_relative_position, reverse = .data$reversed),
      end = adjusted_xmax(.data$cluster_start, .data$cluster_end, .data$position, new_relative_position, reverse = .data$reversed)
    )

  return(data)
}
