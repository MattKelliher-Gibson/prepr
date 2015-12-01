#' Strips all White Space from character string
#'
#' \code{StripWhite} removes all extra spaces from before and after a character string
#'
#' @param data a data.frame/data.table/character vector
#' @return same class as imput: data.frame/data.table/character vector
#' @export

#A. Generic

StripWhite <- function(data) {
  UseMethod("StripWhite")
}

#' @export
#' @describeIn StripWhite default

StripWhite.default <- function(data) {
  if (is.character(data)){
    stringr::str_trim(data)
  } else {
    stop(paste(quote(data), "is not a character vector. Class:", class(data)))
  }
}

#' @export
#' @describeIn StripWhite data.frame

StripWhite.data.frame <- function(data) {
	for (i in 1:length(names(data))) {
		if (class(data[[i]]) == "character") {
			data[[i]] <- stringr::str_trim(data[[i]])
		}
	}

	data
}

#' @export
#' @describeIn StripWhite Returned Invisibly

StripWhite.data.table <- function(data) {
  out <- names(data)[vapply(data, is.character, logical(1))]

  for (i in out){
    data[, invisible((i) := stringr::str_trim(get(i)))]
  }
}
