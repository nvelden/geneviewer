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
#' @param padding The room left on either side of each break: a number, as a
#'   percentage of the region shown (without the non-coding regions left out),
#'   or a string in base pairs, e.g. "500bp".
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

  for(i in seq_len(nrow(genes))[-1]){
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

  for(i in seq_len(length(merged_regions) - 1)){
    non_coding_start <- merged_regions[[i]]$end + 1
    non_coding_end <- merged_regions[[i+1]]$start - 1
    gap_length <- non_coding_end - non_coding_start + 1

    if (gap_length >= min_gap_length) {
      non_coding_regions <- rbind(non_coding_regions, data.frame(start=non_coding_start, end=non_coding_end))
    }
  }

  if (is.character(padding)) {
    # In base pairs
    padding_val <- break_padding_bp(padding)
  } else {
    # A share of the region shown: the entire region less the regions left out
    shown_length <- entire_region_length -
      sum(non_coding_regions$end - non_coding_regions$start + 1)
    padding_val <- shown_length * padding / 100
  }
  non_coding_regions$start <- non_coding_regions$start + padding_val
  non_coding_regions$end <- non_coding_regions$end - padding_val
  # A gap too short for the padding gets no break
  non_coding_regions <- non_coding_regions[non_coding_regions$start < non_coding_regions$end, ]
  # Row names would become list names, sent to the chart as an object
  rownames(non_coding_regions) <- NULL

  #convert df to list of lists
  non_coding_regions <-
  apply(non_coding_regions, 1, function(row) {
    as.list(stats::setNames(row, names(row)))
  })

  return(non_coding_regions)
}

#' The base pairs of a padding given as a string, e.g. "500bp"
#' @noRd
break_padding_bp <- function(padding) {
  if (!grepl("^\\s*[0-9]*\\.?[0-9]+\\s*bp\\s*$", padding)) {
    stop("scale_break_padding must be a percentage, e.g. 1, or base pairs, e.g. \"500bp\"")
  }
  as.numeric(sub("\\s*bp\\s*$", "", padding))
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

#' Update Start and End Columns Based on Merged Data
#'
#' This function merges two data frames by a specified column (default is
#' "rowID") and updates the 'start' and 'end' columns in the original data based
#' on values in the updated data. If the updated data contains a non-missing
#' value for 'start' or 'end', it keeps the corresponding value in the
#' original data.
#'
#' @param data A data frame containing the original data.
#' @param updated_data A data frame containing the updated data with the same
#'   'rowID' column.
#' @param by_col A character string specifying the column name to merge the data
#'   frames by. Default is "rowID".
#' @return A data frame with updated 'start' and 'end' columns.
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

  # Update the 'start' and 'end' columns with new values if available
  result$start <- ifelse(!is.na(result$start_new), result$start_new, result$start)
  result$end <- ifelse(!is.na(result$end_new), result$end_new, result$end)

  # Remove the columns with the "_new" suffix
  result <- result[, !grepl("_new$", names(result))]

  return(result)
}


#' Keep colour and pattern options when gene or transcript options are reset
#'
#' `GC_genes()` and `GC_transcript()` replace their options, which would drop
#' colours from `GC_color()` and patterns from `GC_pattern()`. This copies them
#' from the old options unless new colours or a new pattern are given.
#'
#' @param options List, the new options.
#' @param old List or NULL, the options they replace.
#' @return The new options with the old colours and pattern added.
#' @noRd
keep_color_options <- function(options, old) {
  if (is.null(options$colorScheme) && is.null(options$customColors)) {
    options$colorScheme <- old$colorScheme
    options$customColors <- old$customColors
  }
  if (is.null(options$pattern)) {
    options$pattern <- old$pattern
  }
  options
}

#' Set the legend colours to the gene colours
#'
#' @param GC_chart The gene chart object.
#' @param colorScheme,customColors The colours given to `GC_genes()` or
#'   `GC_transcript()`.
#' @return The gene chart object.
#' @noRd
set_legend_colors <- function(GC_chart, colorScheme, customColors) {
  if (is.null(colorScheme) && is.null(customColors)) {
    return(GC_chart)
  }
  GC_chart$x$legend$legendOptions$colorScheme <- colorScheme
  GC_chart$x$legend$legendOptions$customColors <- customColors
  GC_chart
}

#' Keep the settings of the whole chart in a chart made again
#'
#' `GC_transcript()` makes the chart again with `GC_chart()`, with the
#' transcripts as clusters. This keeps the title, legend, theme, events,
#' toolbar and width of the old chart, and its `GC_grid()` margins and
#' direction. Settings per cluster are for the old clusters and are not kept.
#'
#' @param GC_chart The chart made again.
#' @param old_chart The chart before it was made again.
#' @return The gene chart object.
#' @noRd
keep_chart_settings <- function(GC_chart, old_chart) {
  old <- old_chart$x
  GC_chart$x$title <- old$title
  GC_chart$x$legend <- old$legend
  GC_chart$x$theme <- old$theme
  GC_chart$x$events <- old$events
  GC_chart$x$toolbar <- old$toolbar
  GC_chart$width <- old_chart$width

  # GC_grid() margins: left and right are kept when all clusters had the same
  # (a margin for the whole chart); top and bottom are the padding of the chart
  margins <- lapply(old$series, function(series) series$container$margin)
  same_margin <- length(margins) > 0 &&
    all(vapply(margins, identical, logical(1), margins[[1]]))
  margin <- if (same_margin) margins[[1]] else list()
  margin$top <- old$style$paddingTop
  margin$bottom <- old$style$paddingBottom
  if (length(margin) > 0) {
    GC_chart <- GC_grid(GC_chart, margin = margin)
  }
  if (identical(old$graphContainer$direction, "row")) {
    GC_chart <- GC_grid(GC_chart, direction = "row")
  }

  GC_chart
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

#' Options without the NULL and empty values
#'
#' @param options A named list.
#' @return `options` without the elements that are NULL or of length 0.
#' @noRd
compact <- function(options) {
  Filter(function(x) !is.null(x) && length(x) > 0, options)
}

#' Merge new options into the old ones
#'
#' @param old List or NULL, the options set before.
#' @param new List, the new options.
#' @return `new` when there are no old options, otherwise the old options
#'   updated with `new` by `utils::modifyList()`.
#' @noRd
merge_options <- function(old, new) {
  if (is.null(old)) new else utils::modifyList(old, new)
}

#' Add the ... arguments to the options of the i-th selected cluster
#'
#' @param options List, the options of the cluster.
#' @param dots List, the ... arguments; vectors are recycled over the clusters.
#' @param i Integer, the index of the cluster.
#' @return `options` with the ... arguments for cluster `i`.
#' @noRd
recycle_dots <- function(options, dots, i) {
  for (name in names(dots)) {
    options[[name]] <- recycle_at(dots[[name]], i)
  }
  options
}

#' Set the options of a component for the selected clusters
#'
#' @param GC_chart The gene chart object.
#' @param cluster The clusters to update, as for `getUpdatedClusters()`.
#' @param slot Character, the component, e.g. "labels".
#' @param options_for Function of the index of the cluster and its series,
#'   returning the options of the component for that cluster.
#' @return The gene chart object.
#' @noRd
set_cluster_options <- function(GC_chart, cluster, slot, options_for) {
  clusters <- getUpdatedClusters(GC_chart, cluster)
  for (i in seq_along(clusters)) {
    series <- GC_chart$x$series[[clusters[i]]]
    GC_chart$x$series[[clusters[i]]][[slot]] <- options_for(i, series)
  }
  GC_chart
}
