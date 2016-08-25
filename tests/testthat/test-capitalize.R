context("capitalize function")

test_that("capitalize works with data.frame", {
  options(stringsAsFactors = FALSE)
  x <- data.frame(x1 = c("test", "this", "column"), x2 = 1:3, x3 = c("also", "test", "these"))
  x2 <- data.frame(x1 = c("TEST", "THIS", "COLUMN"), x2 = 1:3, x3 = c("ALSO", "TEST", "THESE"))
  expect_equal(capitalize(x), x2)
})

test_that("capitalize throws correct errors", {
  options(stringsAsFactors = FALSE)
  x <- data.table(x1 = c("test", "this", "column"), x2 = 1:3, x3 = c("also", "test", "these"))
  expect_error(capitalize(x), "Please Load dtplyr for data.table")
  expect_error(capitalize(c(1:10)), "Invalid Data Type")
})

# test_that("capitalize works with data.table", {
#   options(stringsAsFactors = FALSE)
#   library(dtplyr)
#   x <- data.frame(x1 = c("test", "this", "column"), x2 = 1:3, x3 = c("also", "test", "these"))
#   x2 <- data.table(x1 = c("TEST", "THIS", "COLUMN"), x2 = 1:3, x3 = c("ALSO", "TEST", "THESE"))
#   expect_equal(capitalize(as.data.table(x)), x2)
# })

test_that("capitalize works with vectors", {
  options(stringsAsFactors = FALSE)
  x <- c("test", "this", "column")
  expect_equal(capitalize(x), c("TEST", "THIS", "COLUMN"))
})
