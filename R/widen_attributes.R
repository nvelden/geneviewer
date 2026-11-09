#' Spread reader metadata into one row per record
#'
#' The readers supply character columns named id, key and value. Preserve the
#' first-seen order of records and keys, missing values and non-syntactic names.
#' A key repeated within a record keeps all its values in one character value,
#' joined with "," in input order, as GFF3 writes several values of one key
#' (B48); missing values are left out, and a cell whose values are all missing
#' is NA.
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

  values <- matrix(NA_character_, nrow = length(ids), ncol = length(keys))
  if (anyDuplicated(cell)) {
    # Factor levels keep cells in first-seen order and values in input order.
    groups <- split(data$value, factor(cell, levels = unique(cell)))
    values[unique(cell)] <- vapply(groups, function(value) {
      value <- value[!is.na(value)]
      if (length(value) == 0) NA_character_ else paste(value, collapse = ",")
    }, character(1), USE.NAMES = FALSE)
  } else {
    values[cell] <- data$value
  }

  columns <- lapply(seq_along(keys), function(j) values[, j])
  keys[is.na(keys)] <- "NA"
  # dplyr is already required; retain the tibble returned by the old readers
  # and reject empty or duplicate column names instead of silently renaming.
  list(id = ids, data = dplyr::as_tibble(stats::setNames(columns, keys),
                                       .name_repair = "check_unique"))
}
