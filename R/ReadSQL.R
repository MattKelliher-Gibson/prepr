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


#' file_location
#' @description \code{file_location} creates a character string using the current working directory
#'  and provided file location.
#'
#' @param f.file character string of folders and file name
#'
#' @note \code{f.file} must begin with a '/' and cannot use '\'
#' @export

file_location <- function(f.file){
  assertthat::assert_that(is.character(f.file), stringr::str_sub(f.file, 1, 1) == "/")

	paste0(getwd(), f.file)
}
