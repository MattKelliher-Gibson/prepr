#' Functions to Cap, Floor and calulate Max values of a \code{data.frame}
#'
#' These function are for manipulating a data.frame for modeling
#'
#' @section Cap:
#' \code{Cap} the maximum value for a \code{data.frame} column
#' \itemize{
#'    \item \bold{data} {a \code{data.frame} or \code{data.table}}
#'    \item \bold{var} {the name of the variable to be capped}
#'    \item \bold{cap} {a number that is will be the max for the column}
#' }
#' \bold{Returns} an object of the same class as \code{data}
#'
#' @section Floor:
#' \code{Floor} sets the minimum value for a \code{data.frame} column
#' \itemize{
#'    \item \bold{data} {a \code{data.frame} or \code{data.table}}
#'    \item \bold{var} {the name of the variable to be floored}
#'    \item \bold{floor} {a number that is will be the min for the column}
#' }
#' \bold{Returns} an object of the same class as \code{data}
#'
#' @section Max:
#' \code{Max} calculates the max value less than than the \code{value}
#' \itemize{
#'    \item \bold{data.var} {a numeric vector (usually from a \code{data.frame})}
#'    \item \bold{value} {the maximum number to limit the vector to (i.e. \code{max(x < value)})}
#'    \item \bold{na_rm} {logical, to indicate if NAs should be removed}
#' }
#' \bold{Returns:} number of max value
#' @name Cap and Floor
NULL

#' @export
#' @rdname Cap and Floor
Cap <- function(data, var, cap) {
  UseMethod("Cap")
}

#' @export
#' @rdname Cap and Floor
Cap.default <- function(data, var, cap) {
  data[[var]] <- ifelse(data[[var]] > cap, cap, data[[var]])
}

#' @export
#' @rdname Cap and Floor
Cap.data.table <- function(data, var, cap) {
  data[get(var) > get(cap), (var) := get(cap)]
}

#' @export
#' @rdname Cap and Floor
Floor <- function(data, var, floor) {
  UseMethod("Floor")
}

#' @export
#' @rdname Cap and Floor
Floor.default <- function(data, var, floor) {
  data[[var]] <- ifelse(data[[var]] < floor, floor, data[[var]])
}

#' @export
#' @rdname Cap and Floor
Floor.data.table <- function(data, var, floor) {
  data[get(var) < get(floor), (var) := get(floor)]
}

#' @export
#' @rdname Cap and Floor
Max <- function(data.var, value, na_rm = TRUE) {
  max(data.var[data.var < value], na.rm = na_rm)
}
