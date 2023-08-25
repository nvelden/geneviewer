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

#' Extract Data from Symbol or Return Vector
#'
#' This function evaluates the provided expression. If the expression is a symbol,
#' it treats the symbol as a column name and extracts the corresponding column from the provided data frame.
#' If the expression is a call that evaluates to a vector (e.g., `as.character(df$column)`),
#' it returns the evaluated vector.
#'
#' @param data A data frame from which a column might be extracted.
#' @param x An expression that could be a symbol representing a column name or a call that evaluates to a vector.
#'
#' @return A vector extracted from the data frame based on the column name represented by the symbol,
#' or a vector if the expression evaluates to one.
#'
#' @examples
#' df <- data.frame(a = 1:5, b = 6:10)
#' select_column_or_return(df, a)
#' select_column_or_return(df, as.character(df$class))
#'
#' @export
select_column_or_return <- function(data, x) {
  x_nm <- deparse(substitute(x))
  if(x_nm %in% names(data)) return(data[x_nm])
  if(is.symbol(substitute(x)) & !exists(x_nm)) {
    stop("x must be a symbol representing a column, or a vector.")
  }
  if(is.vector(x)) return(x)
  stop("x must be a symbol representing a column, or a vector.")
}

#' Get clusters to update from GCVieweR Object
#'
#' This function retrieves the clusters to be updated from a GCVieweR object based on the provided cluster argument.
#' The function checks if the cluster argument is valid and returns the corresponding clusters.
#'
#' @param GCVieweR An object containing the series of clusters.
#' @param cluster A numeric vector or character vector specifying the clusters to be retrieved.
#' If NULL (default), all clusters are returned.
#'
#' @return A character vector of cluster names.
#'
#'
#' @export
getUpdatedClusters <- function(GCVieweR, cluster) {

  # Get the names of the clusters
  clusters <- names(GCVieweR$x$series)

  # If cluster is NULL, update all clusters
  if (is.null(cluster)) {
    cluster <- clusters
  } else if (is.numeric(cluster)) {
    # If cluster is numeric, map the numbers to cluster names
    if (any(cluster > length(clusters) | cluster < 1)) {
      warning("Some cluster numbers provided are out of range. Please check the cluster numbers.")
      return(NULL)
    }
    cluster <- clusters[cluster]
  } else if (!all(cluster %in% clusters)) {
    warning("Some cluster names provided are not valid. Please check the cluster names.")
    return(NULL)
  }

  return(cluster)
}


