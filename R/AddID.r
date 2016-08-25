#' Add a Custom ID Column to Data Frame
#'
#' \code{add_id} adds a sequestional ID column with a custom prefix.
#'
#' @param data \code{data.frame} or \code{data.table}
#' @param prefix Chacter string that appears befoer the number
#' @param pad_number Integer indicating the total digits (not inlcluding prefix)
#' @param name Character string for name of the variable
#'
#' @note For \code{data.table} load package \code{dtplyr}
#'
#' @examples
#' foo <- data.frame(a = c(1:10), b = c(a:z))
#' foo <- AddID(foo, "test")
#'
#' foo2 <- data.frame(a = c(1:10), b = c(a:z))
#' foo2 <- AddID(foo2, "PRE", pad_number = 3, name = "Tag")
#'
#' @importFrom magrittr %>%
#' @export

add_id <- function(data, prefix, pad_number = 10, name = "ID"){
  if("data.table" %in% class(data)){
    if(!R.utils::isPackageLoaded("dtplyr")){
      stop("Please Load dtplyr for data.table")
    }
  }

  dots <- list(~paste(prefix, stringr::str_pad(c(1:nrow(data)), pad_number, pad = 0), sep = "-"))

  to_return <- data %>% dplyr::mutate_(.dots = setNames(dots, name))
  to_return
}
