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

#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`
