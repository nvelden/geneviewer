#' Normalize Gene Coordinates with Optional Gaps
#'
#' This function normalizes the genomic coordinates of a set of genes along the
#' range of the cluster, optionally preserving the original gene lengths and
#' introducing gaps between genes.
#'
#' @param data A data frame containing the gene coordinates. It must include
#'   columns `start` and `end` representing the start and end positions of each
#'   gene.
#' @param gap A numeric value between 0 and 1 specifying the proportion of the
#'   total length to be used as the gap between genes. If `NULL`, the gaps are
#'   calculated based on the actual spacing between the genes in the original
#'   data.
#' @param preserve_gene_length A logical value indicating whether to preserve
#'   the original gene lengths. If `TRUE`, the gene lengths remain unchanged; if
#'   `FALSE`, all genes are assigned the same length.
#'
#' @return A data frame with normalized gene coordinates, including the updated
#'   `start` and `end` positions.
#' @importFrom dplyr mutate arrange lead if_else select
#' @importFrom rlang .data
#' @importFrom utils head
#' @noRd
normalize_gc_positions <- function(data, gap = NULL, preserve_gene_length = TRUE) {

  data$original_order <- seq_len(nrow(data))

  data <- data %>%
    dplyr::mutate(
      is_reverse = .data$end < .data$start,
      actual_start = pmin(.data$start, .data$end),
      actual_end = pmax(.data$start, .data$end)
    ) %>%
    dplyr::arrange(.data$actual_start)

  total_length <- max(data$actual_end) - min(data$actual_start)
  num_genes <- nrow(data)

  if (is.null(gap)) {
    data <- data %>%
      dplyr::mutate(gene_gap = dplyr::lead(.data$actual_start) - .data$actual_end)
    total_gap <- sum(data$gene_gap, na.rm = TRUE)
  } else {
    total_gap <- total_length * gap * (num_genes - 1)
  }

  adjusted_total_length <- total_length - total_gap

  if (adjusted_total_length <= 0) {
    warning("The specified gap is too large, resulting in zero or negative gene lengths. Please reduce the gap.")
    return(data)
  }

  mean_gene_length <- adjusted_total_length / num_genes
  data <- data %>%
    dplyr::mutate(gene_length = if (preserve_gene_length) abs(.data$actual_end - .data$actual_start) else mean_gene_length)

  data <- data %>%
    dplyr::arrange(.data$actual_start) %>%
    dplyr::mutate(
      new_start = if (preserve_gene_length) {
        min(.data$actual_start) + cumsum(c(0, utils::head(.data$gene_length + total_gap / nrow(data), -1)))
      } else {
        min(.data$actual_start) + (dplyr::row_number() - 1) * (mean_gene_length + total_gap / nrow(data))
      },
      new_end = .data$new_start + .data$gene_length,
      start = dplyr::if_else(.data$is_reverse,
                             round(.data$new_end),
                             round(.data$new_start)),
      end = dplyr::if_else(.data$is_reverse,
                           round(.data$new_start),
                           round(.data$new_end))
    )

  data <- data %>%
    dplyr::arrange(.data$original_order) %>%
    dplyr::select(
      -.data$new_start, -.data$new_end, -.data$actual_start, -.data$actual_end,
      -.data$original_order, -.data$is_reverse, -.data$gene_length)

  data$gene_gap <- NULL

  return(data)
}


