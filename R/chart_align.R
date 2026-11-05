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
  spec <- spec_args(match.call(), environment())
  state <- chart_state(GC_chart)
  if (is.null(state$cluster)) {
    warning("Could not align selected gene. Please define cluster in the GC_chart function.")
    return(GC_chart)
  }

  if (length(id) != 1 || length(id_column) != 1) {
    stop("id and id_column must be single values")
  }

  if (!(id_column %in% colnames(state$data))) {
    warning(paste("Column", id_column, "not found in the data."))
    return(GC_chart)
  }

  if (!(id %in% state$data[[id_column]])) {
    warning(paste("ID", id, "not found in the", id_column, "column."))
    return(GC_chart)
  }

  # The library sets the scale of each cluster from the alignment; it needs
  # the gene once per cluster
  genes <- state$data[!is.na(state$data[[id_column]]) & state$data[[id_column]] == id, ]
  if (anyDuplicated(genes[[state$cluster]])) {
    warning(paste("The selected gene", id, "appears multiple times in some clusters.",
                  "Please make sure to use a unique gene id."))
  } else if (!(is.character(align) && length(align) == 1 && align %in% c("left", "right", "center"))) {
    warning("Alignment should be 'left', 'right', or 'center'. Defaulting to 'left'.")
  }

  add_spec(GC_chart, "align", spec, mode = "replace")
}

#' Normalize Gene Clusters in a Genomic Chart
#'
#' This function normalizes the genomic coordinates of a set of genes within a
#' cluster, ensuring that the genes are evenly spaced along the entire range of
#' the cluster. The function allows for the option to preserve the original gene
#' lengths and to introduce customized gaps between the genes.
#'
#' @param GC_chart A chart object containing genomic data and clustering
#'   information, from [GC_chart()].
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

  state <- chart_state(GC_chart)

  # Check if 'cluster' column exists if the cluster parameter is provided
  if (is.null(state$cluster) && !is.null(cluster)) {
    warning("Could not normalize selected genes. Please define cluster in the GC_chart function.")
    return(GC_chart)
  }

  if (is.null(group) && !is.null(state$group)){
    group <- state$group
  }

  if (!is.null(group) && length(group) != 1) {
    stop("group must be a single column name")
  }

  if (!is.null(group) && !(group %in% names(state$data))) {
    stop("group column not found in data")
  }

  clusters <- getUpdatedClusters(state$clusters, cluster)
  data <- state$data

  for (i in seq_along(clusters)) {

    cluster_name <- clusters[i]
    cluster_data <- cluster_rows(state, cluster_name)

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

    data <- update_cluster_data(data, cluster_data)

  }

  # The spec holds the shifted positions, also in the columns given as
  # start and end
  for (field in c("start", "end")) {
    data[[state[[field]]]] <- data[[field]]
  }
  GC_chart$x$spec$data <- data

  return(GC_chart)
}
