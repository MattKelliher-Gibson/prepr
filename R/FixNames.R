#' Cleans and parses combined name fields
#'
#' \code{FixName} prosses a raw name column, parses the name
#'   into, at most, 5 words, and assigns words to Prefix, First,
#'   Middle, Last, and Suffix.
#'  Only for \code{\link[data.table]{data.table}} and all results returned invisibly
#'
#' @param .data \code{\link[data.table]{data.table}}
#' @return \code{data.table} invisibly
#'
#' @export

FixName <- function(.data, .name_field) {
  strip_white(.data)

  capitalize(.data)

  .data[, Final_Name := CleanName(get(.name_field))]

  .data[, WordCount := CountWords(Final_Name)]

  ParseWords(.data, name_field = Final_Name)

  ParsePrefix(.data)

  ParseSuffix(.data)

  ParseFirst(.data)

  ParseLast(.data)

  PareseMiddle(.data)
}
