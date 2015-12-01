#' Puts Vector in Title Case
#'
#' \code{TitleCase} capitalizes the first letter of each word and returns the other in lowercase
#'
#' @param v a character vector
#' @return a character vecto
#' @export
TitleCase <- function(v){
  s <- strsplit(v, " ", fixed = TRUE)[[1]]

  paste(toupper(substring(s, 1, 1)), tolower(substring(s, 2)), sep = "", collapse = " ")
}
