#' Puts Vector in Title Case
#'
#' \code{TitleCase} capitalizes the first letter of each word and returns the others in lowercase
#'
#' @param v Character vector
#' @return A character vector
#' @export
TitleCase <- function(v){
  if(length(v) > 1){
    warning("Length is greater than 1, only first element will be returned.")
  }
  s <- strsplit(v, " ", fixed = TRUE)[[1]]

  paste(toupper(substring(s, 1, 1)), tolower(substring(s, 2)), sep = "", collapse = " ")
}
