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

#' Calculate the Aligned Range of Each Cluster
#'
#' This function computes the start and end of the scale of each cluster so that
#' the selected gene sits at the same place in every cluster. The clusters are
#' shifted, not stretched: every range has the same length, the longest stretch
#' left of the gene plus the longest stretch right of it, so all clusters keep
#' the same scale. For reversed clusters left and right are swapped, as they are
#' drawn from their end to their start.
#'
#' @param data A dataframe containing gene data with at least the cluster, group,
#'   start and end columns. Start must be smaller than or equal to end.
#' @param cluster The name of the column in `data` representing cluster identifiers.
#' @param group The name of the column in `data` representing group identifiers.
#' @param gene The specific gene name to align on.
#' @param align A string indicating the side of the gene to align on. Valid values
#'   are "left", "right", or "center". Defaults to "left".
#' @param reversed_clusters A vector of cluster names where the clusters are considered
#'   to be in the reversed orientation.
#' @return A dataframe with one row per cluster that contains the gene, with the
#'   cluster and the new `start` and `end` of its scale, or NULL when the gene is
#'   not unique within a cluster.
#' @importFrom dplyr group_by summarise ungroup filter left_join mutate n sym
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

  cluster_min_max <- data %>%
    dplyr::group_by(!!sym(cluster)) %>%
    dplyr::summarise(cluster_start = min(c(.data$start, .data$end)), cluster_end = max(c(.data$start, .data$end))) %>%
    dplyr::ungroup()

  genes <- data[!is.na(data[[group]]) & data[[group]] == gene, c(cluster, "start", "end")]

  if (anyDuplicated(genes[[cluster]])) {
    warning(paste("The selected gene", gene, "appears multiple times in some clusters.",
                  "Please make sure to use a unique gene id."))
    return(NULL)
  }

  genes <- dplyr::left_join(genes, cluster_min_max, by = cluster)
  genes$reversed <- genes[[cluster]] %in% reversed_clusters
  genes$position <- mapply(function(start, end, cluster_start, reversed) {
    get_position_and_offset(align, start, end, cluster_start, reversed)$position
  }, genes$start, genes$end, genes$cluster_start, genes$reversed)

  # The room each cluster needs left and right of the gene, as it is drawn
  below <- genes$position - genes$cluster_start
  above <- genes$cluster_end - genes$position
  left <- ifelse(genes$reversed, above, below)
  right <- ifelse(genes$reversed, below, above)

  genes$start <- genes$position - ifelse(genes$reversed, max(right), max(left))
  genes$end <- genes$position + ifelse(genes$reversed, max(left), max(right))

  genes[, c(cluster, "start", "end")]
}
