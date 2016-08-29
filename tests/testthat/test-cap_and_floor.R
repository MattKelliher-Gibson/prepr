context("Cap and Floor functions")

options(stringsAsFactors = FALSE)

test_that("cap function works with vector", {
  x <- c(1:10)
  expect_equal(cap(x, 9), c(1:9, 9))
  expect_equal(cap(x, 2), c(1, rep(2, 9)))
  expect_equal(max(cap(x, 5)), 5)
  expect_error(cap("A", 2), "data is not a numeric or integer vector")
  expect_error(cap(x, "A"), "cap is not a numeric or integer vector")
})

test_that("floor function works with vector", {
  x <- c(1:10)
  expect_equal(floor(x, 2), c(2, 2:10))
  expect_equal(floor(x, 9), c(rep(9,9), 10))
  expect_equal(min(floor(x, 5)), 5)
  expect_error(floor("A", 2), "data is not a numeric or integer vector")
  expect_error(floor(x, "a"), "floor is not a numeric or integer vector")
})

test_that("cap_all and floor_all only affects numerical columns", {
  options(stringsAsFactors = FALSE)
  x <- data.frame(x = 1:10, y = c(rep("test", 10)), z = 5:14)
  expect_equal(cap_all(x, 5)$x, c(1:4, rep(5, 6)))
  expect_equal(cap_all(x, 5)$y, c(rep("test", 10)))
  expect_equal(cap_all(x, 5)$z, c(rep(5, 10)))
  expect_equal(floor_all(x, 5)$x, c(rep(5, 5), 6:10))
  expect_equal(floor_all(x, 5)$y, c(rep("test", 10)))
  expect_equal(floor_all(x, 6)$z, c(6, 6:14))
})

test_that("cap_all and floor_all give data.table error", {
  options(stringsAsFactors = FALSE)
  x <- data.table(x = 1:10, y = c(rep("test", 10)), z = 5:14)
  expect_error(cap_all(x), "Please Load dtplyr for data.table")
  expect_error(floor_all(x), "Please Load dtplyr for data.table")
})

# test_that("data.table works", {
#   library(dtplyr)
#   x <- data.table(x = 1:10, y = c(rep("test", 10)), z = 5:14)
#   x2 <- data.table(x = c(1:6, rep(6, 4)), y = c(rep("test", 10)), z = c(5, rep(6, 9)))
#   expect_equal(cap_all(x, cap = 6), x2)
# })
