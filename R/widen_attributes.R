#' Spread reader metadata into one row per record
#'
#' The readers supply character columns named id, key and value. Preserve the
#' first-seen order of records and keys, missing values and non-syntactic names.
#' Repeated keys within a record retain all values in list columns, with NULL
#' for absent keys, as in the previous pivot_wider() implementation.
#' Return record IDs separately from the metadata so any key, including id,
#' remains available to the file format.
#' @noRd
widen_attributes <- function(data) {
  ids <- unique(data$id)
  keys <- unique(data$key)
  row <- match(data$id, ids)
  col <- match(data$key, keys)
  # Use doubles so large tables do not overflow integer cell indices.
  cell <- row + (as.double(col) - 1) * length(ids)

  if (anyDuplicated(cell)) {
    warning("Values from `value` are not uniquely identified; output will contain list-cols.",
            call. = FALSE)
    values <- matrix(vector("list", length(ids) * length(keys)), nrow = length(ids))
    # Factor levels keep cells in first-seen order and values in input order.
    groups <- split(data$value, factor(cell, levels = unique(cell)))
    values[unique(cell)] <- unname(groups)
  } else {
    values <- matrix(NA_character_, nrow = length(ids), ncol = length(keys))
    values[cell] <- data$value
  }

  columns <- lapply(seq_along(keys), function(j) values[, j])
  keys[is.na(keys)] <- "NA"
  # dplyr is already required; retain the tibble returned by the old readers
  # and reject empty or duplicate column names instead of silently renaming.
  list(id = ids, data = dplyr::as_tibble(stats::setNames(columns, keys),
                                       .name_repair = "check_unique"))
}
