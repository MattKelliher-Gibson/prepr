#' Functions that are similar to excel functions
#'
#' \code{right} and \code{left} act like Right and Left excel functions
#' \code{ifna} replaces NAs with the provided value
#'
#' @param vec1 Charater vector
#' @param chars Number of characters to substrate
#'
#' @param var1 Vector to check if \code{NA}
#' @param var2 Value to replace \code{NA}
#'
#' @name excel_functions
#' @export
right <- function(vec1, chars){
  assertthat::assert_that(is.character(vec1), is.numeric(chars), chars > 0)

	chars1 <- chars*-1
	stringr::str_sub(vec1, start = chars1)
}

#' @rdname excel_functions
#' @export
left <- function(vec1, chars){
  assertthat::assert_that(is.character(vec1), is.numeric(chars), chars > 0)

  stringr::str_sub(vec1, start = 1, end = chars)
}

#' @rdname excel_functions
#' @export
ifna <- function(var1, var2){
  assertthat::assert_that(length(var1) > 1)

	if(class(var1)[1] %like% "POSIX."){
		k.class <- class(var1)
	}

	to_return <- ifelse(is.na(var1), var2, var1)

	if(class(var1)[1] %like% "POSIX.") {
		class(to_return) <- k.class
	}

	to_return
}

