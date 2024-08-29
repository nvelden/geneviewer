#' Adjust Gene Coordinates with Custom Gap Scaling
#'
#' This function adjusts the start and end positions of genes in a data frame
#' by modifying the gap between a specified gene and the subsequent genes.
#' The gap can be increased or decreased based on the provided factor.
#'
#' @param data A data frame containing the gene coordinates. It must include
#'   columns `start` and `end` representing the start and end positions of each
#'   gene, and the `group` column (if specified) for identifying the gene by name.
#' @param group identifies the column name containing gene identifiers.
#' @param gene_name The name of the gene after which the gap should be adjusted.
#'   This is matched against the `group` column.
#' @param factor A numeric value representing the scaling factor for the gap.
#'   A value of 1 keeps the gap unchanged, values greater than 1 increase the gap,
#'   and values less than 1 decrease the gap.
#'
#' @return A data frame with updated `start` and `end` positions, where the gap
#'   after the specified gene has been adjusted according to the `factor`.
#' @importFrom dplyr mutate row_number if_else select arrange
#' @importFrom rlang .data
#' @noRd
custom_gaps <- function(data, group, gene_name, factor = 2) {

  data$original_order <- seq_len(nrow(data))

  data <- data %>%
    dplyr::mutate(
      is_reverse = .data$end < .data$start,
      actual_start = pmin(.data$start, .data$end),
      actual_end = pmax(.data$start, .data$end),
    ) %>%
    dplyr::arrange(.data$actual_start)

  # Find the index of the gene after which the gap should be increased
  gene_index <- which(data[[group]] == gene_name)

  if (length(gene_index) == 0) {
    warning(paste(gene_name, "not found in the specified group column."))
    return(data)
  }

  # Calculate the current gap between the specified gene and the next one
  current_gap <- data$start[gene_index + 1] - data$end[gene_index]

  # Calculate the new gap (factor times the current gap)
  new_gap <- (factor * current_gap) - current_gap

  if (!"original_start" %in% names(data)) {
    data$original_start <- data$start
  }
  if (!"original_end" %in% names(data)) {
    data$original_end <- data$end
  }

  # Adjust the start and end positions of the subsequent genes
  data <- data %>%
    dplyr::mutate(
      new_start = ifelse(dplyr::row_number() > gene_index,
                     .data$actual_start + new_gap,
                     .data$actual_start),
      new_end = ifelse(dplyr::row_number() > gene_index,
                   .data$actual_end + new_gap,
                   .data$actual_end),
      start = dplyr::if_else(.data$is_reverse,
                             .data$new_end,
                             .data$new_start),
      end = dplyr::if_else(.data$is_reverse,
                           .data$new_start,
                           .data$new_end)
    )

  data <- data %>%
    dplyr::arrange(.data$original_order) %>%
    dplyr::select(-.data$actual_start, -.data$actual_end, -.data$original_order,
                  -.data$new_start, -.data$new_end, -.data$is_reverse)

  return(data)
}

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

  if (!all(c("start", "end") %in% colnames(data))) {
    warning("The data does not contain 'start' or 'end' columns.")
    return(data)
  }

  data$original_order <- seq_len(nrow(data))

  data <- data %>%
    dplyr::mutate(
      is_reverse = .data$end < .data$start,
      actual_start = pmin(.data$start, .data$end),
      actual_end = pmax(.data$start, .data$end),
      original_start = .data$start,
      original_end = .data$end
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

  # if (adjusted_total_length <= 0) {
  #  warning("The specified gap is too large, resulting in zero or negative gene lengths. Please reduce the gap.")
  #  return(data)
  # }

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
