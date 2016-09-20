library(prepr)
context("Add ID")

test_that("add_id returns new column", {
  xx <- data.frame(x = 1:5, y = c("a", "b", "c", "d", "e"), stringsAsFactors = FALSE)
  .id <- c("test-001", "test-002", "test-003", "test-004", "test-005")
  xx2 <- data.frame(x = 1:5, y = c("a", "b", "c", "d", "e"), ID = .id, stringsAsFactors = FALSE)
  expect_equal(add_id(xx, "test", 3), xx2)
  expect_equal(add_id(xx, "test", 3)[, "ID"], .id)
})

test_that("add_id new column is character", {
  xx <- data.frame(x = 1:5, y = c("a", "b", "c", "d", "e"), stringsAsFactors = FALSE)
  xx2 <- add_id(xx, "t", 2)
  expect_true(is.character(xx2$ID))
})

test_that("add_id throws errors", {
  library(data.table)
  xx <- data.frame(x = 1:5, y = c("a", "b", "c", "d", "e"), stringsAsFactors = FALSE)
  expect_error(add_id(as.data.table(xx), "t", 2), "Please Load dtplyr for data.table")
  expect_error(add_id(c(1:10), "test", 5))
})
