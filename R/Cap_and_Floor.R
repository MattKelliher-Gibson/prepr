#' Functions to Cap, Floor and calulate Max values of a \code{data.frame}
#'
#' These function are for manipulating a data.frame for modeling
#'
#' @param data Numeric vector
#' @param cap Number of max value
#' @param floor Number of min value
#' @section Max:
#' \code{Max} calculates the max value less than than the \code{value}
#' \itemize{
#'    \item \bold{data.var} {a numeric vector (usually from a \code{data.frame})}
#'    \item \bold{value} {the maximum number to limit the vector to (i.e. \code{max(x < value)})}
#'    \item \bold{na_rm} {logical, to indicate if NAs should be removed}
#' }
#' \bold{Returns:} number of max value
#' @name Cap_and_Floor
NULL

#' @export
#' @rdname Cap_and_Floor
cap <- function(data, cap) {
  if(!is.numeric(data)){
    stop("Not a numeric vector.")
  }
  to_return <- ifelse(data > cap, cap, data)
  to_return
}

#' @export
#' @rdname Cap_and_Floor
cap_all <- function(data, cap){
  UseMethod("cap_all")
}

#' @export
cap_all.default <- function(data, cap){
  to_return <- dplyr::mutate_if(data, is.numeric, prepr::cap, cap = cap)
  to_return
}

#' @export
cap_all.data.table <- function(data, cap){
  if(!R.utils::isPackageLoaded("dtplyr")){
    stop("Please Load dtplyr for data.table")
  }

  to_return <- dplyr::mutate_if(data, is.numeric, prepr::cap, cap = cap)
  to_return
}

#' @export
#' @rdname Cap_and_Floor
floor <- function(data, floor) {
  if(!is.numeric(data)){
    stop("Not a numeric vector.")
  }
  to_return <- ifelse(data < floor, floor, data)
  to_return
}

#' @export
#' @rdname Cap_and_Floor
floor_all <- function(data, floor){
  UseMethod("floor_all")
}

#' @export
floor_all.default <- function(data, floor){
  to_return <- dplyr::mutate_if(data, is.numeric, prepr::floor, floor = floor)
  to_return
}

#' @export
floor_all.data.table <- function(data, floor){
  if(!R.utils::isPackageLoaded("dtplyr")){
    stop("Please Load dtplyr for data.table")
  }

  to_return <- dplyr::mutate_if(data, is.numeric, prepr::floor, floor = floor)
  to_return
}

#' @export
#' @rdname Cap_and_Floor
Max <- function(data.var, value, na_rm = TRUE) {
  max(data.var[data.var < value], na.rm = na_rm)
}
