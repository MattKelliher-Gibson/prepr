context("Processing Full Name Strings")

.prefix <- "Mr"
.first <- "John"
.middle <- "Quincy"
.middle_initial <- "Q"
.last2 <- "Van"
.last <- "Adams"
.suffix <- "Jr"

.full <- list(prefix = .prefix, first = .first, middle = .middle,
              middle_initial = .middle_initial, last = .last, suffix = .suffix)

.null <- list(prefix = NA_character_, first = NA_character_, middle = NA_character_,
              middle_initial = NA_character_, last = NA_character_, suffix = NA_character_)

test_that("1 word works", {
  x <- .null
  x$last <- .last
  expect_equal(process_words(.last), x)
})

test_that("2 words works", {
  x <- .null
  x[c(1,5)] <- c(.prefix, .last)
  expect_equal(process_words(.prefix, .last), x)
  expect_equal(two_words(.prefix, .last), x)

  x[c(1:2)] <- c(NA_character_, .first)
  expect_equal(process_words(.first, .last), x)
  expect_equal(two_words(.first, .last), x)
})

test_that("3 words works", {
  x <- .null
  x[c(1, 2, 5)] <- c(.prefix, .first, .last)
  expect_equal(process_words(.prefix, .first, .last), x)
  expect_equal(three_words(.prefix, .first, .last), x)

  x[c(2, 5)] <- c(NA_character_, paste(.last2, .last))
  expect_equal(process_words(.prefix, .last2, .last), x)
  expect_equal(three_words(.prefix, .last2, .last), x)

  x[5:6] <- c(.last, .suffix)
  expect_equal(process_words(.prefix, .last, .suffix), x)
  expect_equal(three_words(.prefix, .last, .suffix), x)

  x[c(1, 2, 5, 6)] <- c(NA_character_, .first, paste(.last2, .last), NA_character_)
  expect_equal(process_words(.first, .last2, .last), x)
  expect_equal(three_words(.first, .last2, .last), x)

  x[c(3:5)] <- c(.middle, .middle_initial, .last)
  expect_equal(process_words(.first, .middle, .last), x)
  expect_equal(three_words(.first, .middle, .last), x)

  x$middle <- NA_character_
  expect_equal(process_words(.first, .middle_initial, .last), x)
  expect_equal(three_words(.first, .middle_initial, .last), x)

  x[c(4, 6)] <- c(NA_character_, .suffix)
  expect_equal(process_words(.first, .last, .suffix), x)
  expect_equal(three_words(.first, .last, .suffix), x)
})

test_that("4 words works", {
  x <- .null
  x[1:5] <- c(.prefix, .first, .middle, .middle_initial, .last)
  expect_equal(process_words(.prefix, .first, .middle, .last), x)
  expect_equal(four_words(.prefix, .first, .middle, .last), x)

  x$middle <- NA_character_
  expect_equal(process_words(.prefix, .first, .middle_initial, .last), x)
  expect_equal(four_words(.prefix, .first, .middle_initial, .last), x)

  x[4:5] <- c(NA_character_, paste(.last2, .last))
  expect_equal(process_words(.prefix, .first, .last2, .last), x)
  expect_equal(four_words(.prefix, .first, .last2, .last), x)

  x[c(5, 6)] <- c(.last, .suffix)
  expect_equal(process_words(.prefix, .first, .last, .suffix), x)
  expect_equal(four_words(.prefix, .first, .last, .suffix), x)

  x[c(2, 5)] <- c(NA_character_, paste(.last2, .last))
  expect_equal(process_words(.prefix, .last2, .last, .suffix), x)
  expect_equal(four_words(.prefix, .last2, .last, .suffix), x)

  x[1:5] <- c(NA_character_, .first, .middle, .middle_initial, .last)
  expect_equal(process_words(.first, .middle, .last, .suffix), x)
  expect_equal(four_words(.first, .middle, .last, .suffix), x)

  x$middle <- NA_character_
  expect_equal(process_words(.first, .middle_initial, .last, .suffix), x)
  expect_equal(four_words(.first, .middle_initial, .last, .suffix), x)

  x[4:5] <- c(NA_character_, paste(.last2, .last))
  expect_equal(process_words(.first, .last2, .last, .suffix), x)
  expect_equal(four_words(.first, .last2, .last, .suffix), x)
})

test_that("5 words works", {
  x <- .full
  expect_equal(process_words(.prefix, .first, .middle, .last, .suffix), x)

  x$middle <- NA_character_
  expect_equal(process_words(.prefix, .first, .middle_initial, .last, .suffix), x)
})
