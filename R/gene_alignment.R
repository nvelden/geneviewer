#' Calculate the Position of a Gene to Align On
#'
#' This function determines the position of a gene to align the clusters on,
#' based on the specified alignment (left, right, or center). It also accounts
#' for the orientation of the cluster (normal or reversed).
#'
#' @param align A string indicating the align of the gene to consider for position calculation.
#'   Valid values are "left", "right", or "center". The function will default to "left"
#'   if an invalid align is provided.
#' @param start The start coordinate of the gene.
#' @param end The end coordinate of the gene.
#' @param reversed A logical value indicating whether the cluster is in a reversed orientation.
#'
#' @return The position of the gene to align on.
#' @noRd
get_align_position <- function(align, start, end, reversed) {
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

  position
}

#' Calculate the Aligned Range of Each Cluster
#'
#' This function computes the start and end of the scale of each cluster so that
#' the selected gene sits at the same place in every cluster. The clusters are
#' shifted, not stretched: every range has the same length, the longest stretch
#' left of the gene plus the longest stretch right of it, so all clusters keep
#' the same scale. Scale breaks are not counted: a cluster with breaks gets a
#' range that much longer, which the breaks take out again. For reversed
#' clusters left and right are swapped, as they are drawn from their end to
#' their start.
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
#' @param breaks A list by cluster name of the scale breaks of that cluster,
#'   each a list with a `start` and an `end`.
#' @return A dataframe with one row per cluster that contains the gene, with the
#'   cluster and the new `start` and `end` of its scale, or NULL when the gene is
#'   not unique within a cluster.
#' @importFrom dplyr group_by summarise ungroup filter left_join mutate n sym
#' @importFrom rlang .data
#' @noRd
adjust_range <- function(data, cluster, group, gene, align = "left", reversed_clusters = NULL,
                         breaks = list()){

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
  genes$position <- mapply(function(start, end, reversed) {
    get_align_position(align, start, end, reversed)
  }, genes$start, genes$end, genes$reversed)

  # The length of the scale breaks of each cluster below and above the gene
  gaps <- function(from, to, cl) {
    sum(vapply(breaks[[as.character(cl)]], function(gap) {
      max(0, min(gap$end, to) - max(gap$start, from))
    }, numeric(1)))
  }
  gaps_below <- mapply(gaps, genes$cluster_start, genes$position, genes[[cluster]])
  gaps_above <- mapply(gaps, genes$position, genes$cluster_end, genes[[cluster]])

  # The room each cluster needs left and right of the gene, as it is drawn
  below <- genes$position - genes$cluster_start - gaps_below
  above <- genes$cluster_end - genes$position - gaps_above
  left <- ifelse(genes$reversed, above, below)
  right <- ifelse(genes$reversed, below, above)

  genes$start <- genes$position - ifelse(genes$reversed, max(right), max(left)) - gaps_below
  genes$end <- genes$position + ifelse(genes$reversed, max(left), max(right)) + gaps_above

  genes[, c(cluster, "start", "end")]
}
