#' Capitalizes all Character Strings
#'
#' Capitalizes all letters in an object.
#'
#' @section Methods:
#' Current classes supported:
#' \itemize{
#'  \item \code{data.frame}
#'  \item \code{data.table}
#'  \item \code{character vector}
#' }
#'
#' @param data \code{data.frame} containing character columns or \code{character vector}
#'
#' @return Object of input class
#'

#' @export
capitalize <- function(data) {
  UseMethod("capitalize")
}

#' @export
#' @describeIn capitalize data.table
capitalize.data.table <- function(data) {
  if(!R.utils::isPackageLoaded("dtplyr")){
    stop("Please Load dtplyr for data.table")
  }

  data2 <- dplyr::mutate_if(data, is.character, toupper)
  data2
}

#' @export
#' @describeIn capitalize data.frame
capitalize.data.frame <- function(data){
  data2 <- dplyr::mutate_if(data, is.character, toupper)
  data2
}

#' @export
#' @describeIn capitalize default
capitalize.default <- function(data){
  if (is.character(data)){
    toupper(data)
  } else {
	  stop("Invalid Data Type")
  }
}
