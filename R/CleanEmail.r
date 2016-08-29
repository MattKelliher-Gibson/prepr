#' Removes Invalid Characters from Email Addresses
#'
#' \code{clean_email} removes invalid characters and phrases
#'  such as "N/A", "NOEMAIL", and " "
#'
#' @param var \code{character} vector of email addresses
#' @return a character vector of the same length
#'

#' @export

clean_email <- function(var) {
	toupper(var) %>%
	stringr::str_replace_all("N/A", " ") %>%
	stringr::str_replace_all("NOEMAIL", " ") %>%
	stringr::str_replace_all("@FAKE.", " ") %>%
  gsub("[^ @.[:alnum:]]", "", .) %>%
	gsub("^ *|(?<= ) | *$", "", ., perl=T)
}
