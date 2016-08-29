#' Custom Assertions
#'
#' @description Assertions for \code{\link{testthat}}. Not exported and not available.
#'
#' @param x object to be checked
#' @param n \code{integer}
#'
#' @section Assertions:
#' \itemize{
#'  \item{\code{is_integer} checks if a number is an integer
#'    (\code{is_integer(5)} returns \code{TRUE})
#'  }
#'  \item{\code{assert_number_greater} checks if \code{x} is great than \code{n}}
#' }
#' @name Custom_Assertions


is_integer <- function(x) {
  if(is.numeric(x)) {
    if(as.integer(x) == x) {
      TRUE
    } else {
      FALSE
    }
  } else {
    FALSE
  }
}


assertthat::on_failure(is_integer) <- function(call, env) {
  paste0(deparse(call$x), " is not an integer")
}

#' @rdname Custom_Assertions
assert_number_greater <- function(x, n) {
  if(x > n) {
    TRUE
  } else {
    FALSE
  }
}


assertthat::on_failure(assert_number_greater) <- function(call, env) {
  paste0(deparse(call$x), " is less than or equal to ", deparse(call$n))
}
