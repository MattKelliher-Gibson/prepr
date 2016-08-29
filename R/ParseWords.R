#' Parse a character column into several columns
#'
#' \code{ParseWords} separates one character column (usually names)
#'  into several columns with sequential names.
#'  Originally designed for Parsing Name Fields.
#'
#' @param data \code{data.frame}
#' @param total_words \code{integer} total new columns
#' @param name_field \code{string} of column with names
#' @param var.name \code{string} for prefix to new columns names
#' @return \code{data.frame} of the same class as \code{data}
#' @export
#'
#' @examples
#'  options(stringsAsFactors = FALSE)
#'  x <- data.frame(text = c("Mike t Martin", "John D Rockefeller", "Daniel Tiger", "jean claude van damme"))
#'
#'  # Seperate into 3 columns: name1, name2, name3
#'  ParseWords(x, 3, "text", "name")

ParseWords <- function(data, total_words = 5, name_field = NULL, var_name = "word") {
  assertthat::assert_that(is.data.frame(data), assert_number_greater(total_words, 1),
                          is_integer(total_words), is.character(name_field),
                          is.character(var_name))

  tidyr::separate_(data, name_field, paste0(var_name, seq_along(1:total_words)), " ", remove = FALSE, extra = "merge", fill = "right")
}
