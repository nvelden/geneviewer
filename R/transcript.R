
#' Calculate Intron Positions Based on Exons
#'
#' This function takes a dataset containing transcript information and calculates
#' the start and end positions of introns based on the positions of exons. It
#' generates a combined dataframe including both the original exons and the
#' calculated introns.
#'
#' @param data A dataframe containing `start` and `end` positions of transcripts.
#'   Optional columns are `type` (exon or UTR), `transcript` (containing unique
#'   transcript IDs), and `strand` (indicating the direction, forward or reverse,
#'   of each transcript).
#'
#' @return A dataframe with the original exons and the calculated introns, sorted
#'   by transcript and start position. Each row includes the `start`, `end`,
#'   `type` (exon or intron), `transcript`, and `strand` for each segment.
#'
#' @importFrom dplyr mutate filter group_by arrange lead select bind_rows
#' @importFrom rlang .data
#' @export
get_introns <- function(data) {

  if (!"type" %in% colnames(data)) {
    data$type <- "exon"
  }

  if (!"transcript" %in% colnames(data)) {
    data$transcript <- "transcript1"
  }

  if (!"start" %in% colnames(data) || !"end" %in% colnames(data)) {
    stop("The 'start' and 'end' columns are required in the data.")
  }

  # Capture the original order of transcripts
  data <- data %>%
    dplyr::mutate(original_transcript_order = factor(.data$transcript, levels = unique(.data$transcript)))

  intron_results <- data %>%
    dplyr::filter(grepl("exon|utr", .data$type, ignore.case = TRUE))

  # Swap start and end if end is smaller than start
  swap_needed <- intron_results$start > intron_results$end
  intron_results[swap_needed, c("start", "end")] <- intron_results[swap_needed, c("end", "start")]

  # Filter and sort exons, then calculate intron positions
  intron_results <- intron_results %>%
    dplyr::group_by(.data$transcript) %>%
    dplyr::filter(grepl("exon|utr", .data$type, ignore.case = TRUE)) %>%
    dplyr::arrange(.data$transcript, .data$start) %>%
    dplyr::mutate(
      next_start = dplyr::lead(.data$start),
      next_end = dplyr::lead(.data$end),
    ) %>%
    dplyr::filter(!is.na(.data$next_start)) %>%
    dplyr::mutate(
      start = .data$end + 1,
      end = .data$next_start - 1,
      type = "intron"
    ) %>%
    dplyr::select(-.data$next_start, -.data$next_end)

  combined_results <- dplyr::bind_rows(data, intron_results) %>%
    dplyr::arrange(.data$original_transcript_order, .data$start) %>%
    dplyr::select(-.data$original_transcript_order)

  return(combined_results)
}
