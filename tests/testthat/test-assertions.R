context("custom assertions")

x1 <- 1.5
x2 <- "1"
x3 <- "a"
x4 <- data.frame(x = 1)
test_that("Returns True and False Correctly", {
  expect_true(is_integer(1))
  expect_true(is_integer(-1))
  expect_false(is_integer(x1))
  expect_false(is_integer(x2))
  expect_false(is_integer(x3))
  expect_false(is_integer(x4))
})

test_that("Asserts Correctly", {
  expect_error(assertthat::assert_that(is_integer(x1)), "x1 is not an integer")
  expect_error(assertthat::assert_that(is_integer(x2)), "x2 is not an integer")
  expect_error(assertthat::assert_that(is_integer(x3)), "x3 is not an integer")
  expect_error(assertthat::assert_that(is_integer(x4)), "x4 is not an integer")
  expect_silent(assertthat::assert_that(is_integer(1)))
  expect_silent(assertthat::assert_that(is_integer(-1)))
})
