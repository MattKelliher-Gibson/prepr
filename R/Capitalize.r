#' Capitalizes all Character Strings
#'
#' \code{Capitalize} is a generic function that capitalizes
#'  all letters in an object.
#'
#' Current classes supported:
#' \itemize{
#'  \item \code{data.frame}
#'  \item \code{data.table}
#'  \item \code{character} vector
#' }
#'
#' @param data a data.frame/data.table/character vector
#'
#' @return same class as was used for input
#'

#' @export
Capitalize <- function(data) {
  UseMethod("Capitalize")
}

#' @describeIn Capitalize Uses data.frame method (must be assigned)
#' @export
Capitalize.data.table <- function(data) {
  out <- vapply(data, is.character, logical(1))
  .cols <- names(data)[out]

  for (i in .cols){
    data[, invisible((i) := toupper(get(i)))]
  }
}

#' @describeIn Capitalize \code{data.frame} must be assigned
#' @export
Capitalize.data.frame <- function(data){
  out <- vapply(data, is.character, logical(1))

  for(i in names(data)[out]){
    data[[i]] <- toupper(data[[i]])
  }

  data
}

#' @describeIn Capitalize Character and Unsupported Classes
#' @export
Capitalize.default <- function(data){
  if (is.character(data)){
    toupper(data)
  } else {
	  stop("Invalid Data Type")
  }
}
