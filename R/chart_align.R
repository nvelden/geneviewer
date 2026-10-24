#' Align gene clusters
#'
#' This function aligns clusters based on a specified gene id. The clusters are
#' shifted so the gene sits at the same place in each of them, and all clusters
#' keep the same scale: genes of the same length are drawn at the same width.
#' The alignment is kept when [GC_scale()] is called afterwards, also with
#' `axis_type = "range"`.
#'
#' @param GC_chart A chart object containing genomic data along with clustering
#' information.
#' @param id_column The name of the column that contains the gene identifiers.
#' @param id The specific identifier of the gene to be aligned.
#' @param align The alignment method for the gene. Valid values are
#' "left", "right", or "center". Defaults to "left".
#'
#' @return The modified `GC_chart` object with updated genomic coordinates.
#'
#' @examples
#' genes_data <- data.frame(
#'   start = c(10, 90, 130, 170, 210),
#'   end = c(40, 120, 160, 200, 240),
#'   name = c('Gene 1', 'Gene 2', 'Gene 3', 'Gene 4', 'Gene 5'),
#'   group = c('A', 'B', 'C', 'C', 'A'),
#'   cluster =  c(1, 1, 1, 2, 2)
#' )
#'
#'
#' GC_chart(genes_data, group = "group", cluster = "cluster", height = "150px") %>%
#'   GC_align("group", "A", align = "left") %>%
#'   GC_legend(FALSE)
#'
#' @export
GC_align <- function(
    GC_chart,
    id_column,
    id,
    align = "left") {
  if (is.null(GC_chart$x$cluster)) {
    warning("Could not align selected gene. Please define cluster in the GC_chart function.")
    return(GC_chart)
  }

  if (length(id) != 1 || length(id_column) != 1) {
    stop("id and id_column must be single values")
  }

  if (!(id_column %in% colnames(GC_chart$x$data))) {
    warning(paste("Column", id_column, "not found in the data."))
    return(GC_chart)
  }

  if (!(id %in% GC_chart$x$data[[id_column]])) {
    warning(paste("ID", id, "not found in the", id_column, "column."))
    return(GC_chart)
  }

  GC_chart$x$align <- list(id_column = id_column, id = id, align = align)

  apply_align(GC_chart)
}

#' Set the scale of each cluster to the alignment stored by GC_align()
#'
#' Kept apart from GC_align() so GC_scale() can align the clusters again after
#' it has replaced their scale or shifted their genes.
#' @noRd
apply_align <- function(GC_chart) {
  options <- GC_chart$x$align
  if (is.null(options)) {
    return(GC_chart)
  }

  # Swap start and stop for reverse genes
  data <- GC_chart$x$data
  cluster_column <- GC_chart$x$cluster

  # Get reversed clusters
  clusters <- unique(data[[cluster_column]])
  reversed_clusters <- clusters[sapply(clusters, function(cl) {
    isTRUE(GC_chart$x$series[[cl]]$scale$reverse)
  })]

  swapped_indices <- data$start > data$end
  data[swapped_indices, c("start", "end")] <- data[swapped_indices, c("end", "start")]

  # The scale breaks of each cluster, left out of its range
  breaks <- lapply(stats::setNames(nm = as.character(clusters)), function(cl) {
    GC_chart$x$series[[cl]]$scale$breaks
  })

  adjusted_range <- adjust_range(data, cluster_column, options$id_column, options$id,
                                 align = options$align, reversed_clusters, breaks)

  for (i in seq_len(NROW(adjusted_range))) {
    cluster <- as.character(adjusted_range[[cluster_column]][i])
    # Set scale options for each cluster
    GC_chart$x$series[[cluster]]$scale$start <- adjusted_range$start[i]
    GC_chart$x$series[[cluster]]$scale$end <- adjusted_range$end[i]
  }

  return(GC_chart)
}

#' Normalize Gene Clusters in a Genomic Chart
#'
#' This function normalizes the genomic coordinates of a set of genes within a
#' cluster, ensuring that the genes are evenly spaced along the entire range of
#' the cluster. The function allows for the option to preserve the original gene
#' lengths and to introduce customized gaps between the genes.
#'
#' @param GC_chart A chart object containing genomic data and clustering
#'   information. The chart object must include a `series` component with data
#'   for each cluster.
#' @param group Column name containing gene names to apply custom gaps to.
#' @param cluster A vector of cluster names to be normalized. If `NULL`, all
#'   clusters in the `GC_chart` will be normalized.
#' @param normalize A logical value indicating whether to normalize the gene
#'   positions within each cluster. Default is `TRUE`.
#' @param preserve_gene_length A logical vector indicating whether to preserve
#'   the original gene lengths for each cluster. If a single value is provided,
#'   it is recycled for all clusters.
#' @param gap A numeric vector specifying the proportion of the total length to
#'   be used as the gap between genes in each cluster. The value must be between
#'   0 and 1. If a single value is provided, it is recycled for all clusters. If
#'   `NULL`, the gaps are calculated based on the actual spacing between the
#'   genes in the original data.
#' @param custom_gaps A named list where each name corresponds to a gene name and
#'   each value is a numeric factor by which to adjust the gap after that gene.
#'
#' @return The modified `GC_chart` object with normalized gene coordinates for
#'   the specified clusters.
#'
#' @examples
#' genes_data <- data.frame(
#'   start = c(10, 90, 130, 10, 90, 130),
#'   end = c(40, 120, 160, 40, 120, 160),
#'   name = c('Gene 1', 'Gene 2', 'Gene 3','Gene 1', 'Gene 2', 'Gene 3'),
#'   group = c('A', 'B', 'C', 'A', 'B', 'C'),
#'   cluster =  c(1, 1, 1, 2, 2, 2)
#' )
#'
#' GC_chart(genes_data, group = "group", cluster = "cluster", height = "150px") %>%
#'   GC_normalize(
#'     cluster = 2,
#'     preserve_gene_length = TRUE,
#'     gap = NULL
#'  )
#'
#' @export
GC_normalize <-
  function(
    GC_chart,
    group = NULL,
    cluster = NULL,
    normalize = TRUE,
    preserve_gene_length = TRUE,
    gap = NULL,
    custom_gaps = list()
    ) {

  # Check if 'cluster' column exists if the cluster parameter is provided
  if (is.null(GC_chart$x$cluster) && !is.null(cluster)) {
    warning("Could not normalize selected genes. Please define cluster in the GC_chart function.")
    return(GC_chart)
  }

  if (is.null(group) && !is.null(GC_chart$x$group)){
    group <- GC_chart$x$group
  }

  if (!is.null(group) && length(group) != 1) {
    stop("group must be a single column name")
  }

  if (!is.null(group) && !(group %in% names(GC_chart$x$data))) {
    stop("group column not found in data")
  }

  clusters <- getUpdatedClusters(GC_chart, cluster)

  for (i in seq_along(clusters)) {

    cluster_name <- clusters[i]
    cluster_data <- GC_chart$x$series[[cluster_name]]$data

    # Normalize genes
    if(normalize){
    cluster_data <- normalize_gc_positions(
      cluster_data,
      preserve_gene_length = recycle_at(preserve_gene_length, i),
      gap = recycle_at(gap, i))
    }

    if (length(custom_gaps) > 0) {
      if (is.null(group)) {
        warning("Group is NULL. Custom gaps will not be applied because the group column is not specified.")
      } else {
        for (gene_name in names(custom_gaps)) {
          factor <- custom_gaps[[gene_name]]
          cluster_data <- custom_gaps(
            cluster_data,
            group = group,
            gene_name = gene_name,
            factor = factor
          )
        }
      }
    }

    # Update chart and cluster data
    GC_chart$x$series[[cluster_name]]$data <- cluster_data
    GC_chart[["x"]][["series"]][[cluster_name]][["scale"]][["xMin"]] <-
      min(cluster_data$start, cluster_data$end)
    GC_chart[["x"]][["series"]][[cluster_name]][["scale"]][["xMax"]] <-
      max(cluster_data$start, cluster_data$end)

    GC_chart$x$data <- update_cluster_data(GC_chart$x$data, cluster_data)

  }

  return(GC_chart)
}
