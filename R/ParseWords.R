#' Parse a character column into several columns
#'
#' \code{ParseWords} separates one character column (usually names)
#'  into several columns with sequential names.
#'  Originally designed for Parsing Name Fields
#'
#'  @param data a data.frame or data.table or tbl
#'  @param total.words numerical, total new columns
#'  @param var.name character, new columns name prefix
#'  @return data.frame/data.table/tbl of the same class
#'  @export

ParseWords <- function(data, total.words = 5, name_field = NULL, var.name = "word") {
  tidyr::separate(data, name_field, paste0(var.name, 1:total.words), " ", remove = FALSE, extra = "drop")
}
