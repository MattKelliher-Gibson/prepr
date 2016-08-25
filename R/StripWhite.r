#' Strips all White Space from character string
#'
#' \code{strip_white} removes all extra spaces from before and after a character string
#'
#' @inheritParams  capitalize
#' @return Object of input class
#' @export

#A. Generic

strip_white <- function(data) {
  UseMethod("strip_white")
}

#' @export
strip_white.default <- function(data) {
  if (is.character(data)){
    stringr::str_trim(data)
  } else {
    stop(paste(quote(data), "is unsupported class:", class(data)))
  }
}

#' @export
strip_white.data.frame <- function(data) {
  data2 <- mutate_if(data, is.character, stringr::str_trim)
	data2
}

#' @export
strip_white.data.table <- function(data) {
  if(!R.utils::isPackageLoaded("dtplyr")){
    stop("Please Load dtplyr for data.table")
  }

  data2 <- mutate_if(data, is.character, stringr::str_trim)
	data2
}
