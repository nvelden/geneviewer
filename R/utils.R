#' Helper function to validate and clean lists
#'
#' This function takes a list as an argument, checks if it's not NULL and is a list,
#' and then removes any NA or NULL values from it. If the input is not a list,
#' it returns an empty list.
#'
#' @param lst A list that might contain NULL or NA values.
#'
#' @return A list where NULL and NA values have been removed,
#' or an empty list if the input wasn't a list.
cleanList <- function(lst) {
  if (!is.null(lst) && is.list(lst)) {
    return(lst[!sapply(lst, function(x) is.na(x) | is.null(x))])
  } else {
    return(list())
  }
}

#' Extract Data from Symbol or Return Character Vector
#'
#' This function evaluates the provided expression. If the expression is a symbol,
#' it treats the symbol as a column name and extracts the corresponding column from the provided data frame.
#' If the expression is a call that evaluates to a character vector (e.g., `as.character(df$column)`),
#' it returns the evaluated character vector.
#'
#' @param data A data frame from which a column might be extracted.
#' @param x An expression that could be a symbol representing a column name or a call that evaluates to a character vector.
#'
#' @return A vector extracted from the data frame based on the column name represented by the symbol,
#' or a character vector if the expression evaluates to one.
#'
#' @examples
#' df <- data.frame(gene = c("A", "B"), class = c("Oxygenase", "Unknown"))
#' x_expr <- rlang::enexpr(class)
#' select_from_symbol(df, x_expr)
#' x_expr <- rlang::enexpr(as.character(df$class)
#' select_from_symbol(x_expr, x_expr)
#'
#' @importFrom rlang is_symbol as_string enexpr
#' @importFrom magrittr %>%
#' @export
select_from_symbol <- function(data, x) {

  if(inherits(x, "call")) {
    x_val <- eval(x)

    if (is.character(x_val)) {
      return(x_val)
    }
  } else if (rlang::is_symbol(x)) {
    column_name <- rlang::as_string(x)

    # Check if column name exists in the data
    if (!is.null(column_name) && !(column_name %in% colnames(data))) {
      stop(paste("Column", column_name, "not found in data"))
    }

    return(data[[column_name]])
  }
  stop("x must be a symbol representing a column or a character vector.")
}
