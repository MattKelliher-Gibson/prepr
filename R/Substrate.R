#' Functions that are similar to excel functions
#'
#' \code{right} and \code{left} act like Right and Left excel functions
#' \code{ifna} replaces NAs with the provided value
#'
#' @param vec1 a charater vector
#' @param chars number of characters to substrate
#'
#' @export
right <- function(vec1, chars){
	chars1 <- chars*-1
	stringr::str_sub(vec1, start = chars1)
}

#' @export
left <- function(vec1, chars){
  stringr::str_sub(vec1, start = 1, end = chars)
}

#' @export
ifna <- function(var1, var2){
	if(class(var1)[1] %like% "POSIX."){
		k.class <- class(var1)
	}

	to_return <- ifelse(is.na(var1), var2, var1)

	if(class(var1)[1] %like% "POSIX."){
		class(to_return) <- k.class
	}

	return(to_return)
}
