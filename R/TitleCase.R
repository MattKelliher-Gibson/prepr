#' Puts Vector in Title Case
#'
#' \code{TitleCase} capitalizes the first letter of each word and returns the others in lowercase
#'
#' @param v Character vector
#' @return A character vector
#' @export
TitleCase <- function(v){
  s <- strsplit(v, " ", fixed = TRUE)[[1]]

  paste(toupper(substring(s, 1, 1)), tolower(substring(s, 2)), sep = "", collapse = " ")
}
