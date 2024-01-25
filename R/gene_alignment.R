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
