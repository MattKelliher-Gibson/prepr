#' Read in a text file with .sql
#'
#' \code{ReadSQL} reads in text files with .sql extension
#'  for use with SQL packages like sqldf and RODBC
#'
#' @param file character vector of .sql location
#' @return character vector of text
#'
#' @export

ReadSQL <- function(file) {
	paste(readLines(file), collapse = "\n")
}

#' @export

file_location <- function(f.file){
	paste0(getwd(), f.file)
}
