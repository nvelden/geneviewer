
#' Calculate Intron Positions Based on Exons
#'
#' This function takes a dataset containing transcript information and calculates
#' the start and end positions of introns based on the positions of exons. It
#' generates a combined dataframe including both the original exons and the
#' calculated introns.
#' Overlapping, nested, and adjacent exon/UTR intervals are treated as a
#' continuous covered region; only positive gaps become introns.
#'
#' @param data A dataframe containing `start` and `end` positions of transcripts.
#'   Optional columns are `type` (exon or UTR), `transcript` (containing unique
#'   transcript IDs), and `strand` (indicating the direction, forward or reverse,
#'   of each transcript).
#'
#' @return A dataframe with the original exons and the calculated introns, sorted
#'   by transcript and start position. Each row includes the `start`, `end`,
#'   `type` (exon, UTR or intron), `transcript`, and `strand` for each segment.
#'
#' @examples
#' exons <- data.frame(
#'   transcript = "tx1",
#'   type = "exon",
#'   start = c(100, 400, 900),
#'   end = c(250, 600, 1100),
#'   strand = "forward"
#' )
#' get_introns(exons)
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

  # The running end merges overlapping/nested coverage without changing the
  # original exon/UTR rows. Adjacent intervals have no intron between them.
  intron_results <- intron_results %>%
    dplyr::group_by(.data$transcript) %>%
    dplyr::filter(grepl("exon|utr", .data$type, ignore.case = TRUE)) %>%
    dplyr::arrange(.data$transcript, .data$start) %>%
    dplyr::mutate(
      covered_end = cummax(.data$end),
      next_start = dplyr::lead(.data$start)
    ) %>%
    dplyr::filter(!is.na(.data$next_start), .data$next_start > .data$covered_end + 1) %>%
    dplyr::mutate(
      start = .data$covered_end + 1,
      end = .data$next_start - 1,
      type = "intron"
    ) %>%
    dplyr::select(-dplyr::all_of(c("next_start", "covered_end")))

  combined_results <- dplyr::bind_rows(data, intron_results) %>%
    dplyr::arrange(.data$original_transcript_order, .data$start) %>%
    dplyr::select(-"original_transcript_order")

  return(combined_results)
}
