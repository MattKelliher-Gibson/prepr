#' dplyr Join that Fills NAs
#'
#' \code{JoinNA} performs a selected dplyr join and fills ALL NAs
#'
#' @param x a data.frame (left in left_join)
#' @param y a data.frame
#' @param join_type dplyr join
#' @param fill numeric or character to fill NAs
#' @param ... by arguement plus any additional args for join
#' @return data.frame
#'
#' @export

JoinNA <- function(x, y, join_type, fill = 0, ...) {
  join_type(x = x, y = y, by = ...) %>%
  dplyr::mutate_each(funs(replace(., which(is.na(.)), fill)))
  }
