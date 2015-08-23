#' Add a Custom ID Column to Data Frames
#'
#' \code{AddID} adds a sequestional ID column with a custom prefix.
#' This is a generic function that has two methods (defulat and data.table).
#'
#' @param data a data frame
#' @param prefix a chacter string
#' @param pad_number an integer indicating the total digits (inlcluding padding)
#' @param name a character string that appears before the number
#' @return For default a data frame is returned. If a \code{\link[data.table]{data.table}} then
#'  a data table will be returned \strong{invisibly}
#'
#' @examples
#' foo <- data.frame(a = c(1:10), b = c(a:z))
#' foo <- AddID(foo, "test")
#'
#' foo2 <- data.frame(a = c(1:10), b = c(a:z))
#' foo2 <- AddID(foo2, "PRE", .pad_number = 3, .name = "Tag")
#'
#' foo3 <- data.table(a = c(1:10), b = c(a:z))
#' AddID(foo3, "ROW", .name = "Row") #Returned Invisibily
#'
#'
#' @export

AddID <- function(data, prefix, pad_number = 10, name = "ID") UseMethod("AddID")

#' @describeIn AddID data frame must be assigned
#' @export

AddID.default <- function(data, prefix, pad_number = 10, name = "ID"){
	if (is.data.frame(data)) {
		id <- paste(prefix, stringr::str_pad(c(1:nrow(data)), pad_number, pad = 0), sep = "-")

		data.final <- data.frame(id, data, stringsAsFactors = FALSE)

		names(data.final)[1] <- name

		return(data.final)
	} else {
		stop(paste(data, "Not a Data Frame or Data Table"))
  }
}

#' @describeIn AddID data table returned invisibly
#' @import data.table
#' @export

AddID.data.table <- function(data, prefix, pad_number = 10, name = "ID"){

		data[, (name) := paste(prefix, stringr::str_pad(.I, pad_number, pad = 0), sep = "-")]

		setcolorder(data, c(name, names(data)[-length(names(data))]))
}

