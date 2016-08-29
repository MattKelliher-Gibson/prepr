context("ParseWords")

  text <- c("this is a test", "I love my mom", "my name is Daniel Tiger", "and my name is Prince Wednesday")
  x <- data.frame(text = text)

test_that("Parameters properly checked", {
  expect_error(ParseWords("hello world", 6, "text", "word"), "data is not a data.frame")
  expect_error(ParseWords(x, "6", "text", "word"), "total_words is not an integer")
  expect_error(ParseWords(x, 5.5, "text", "word"), "total_words is not an integer")
  expect_error(ParseWords(x, 1, "text", "word"), "total_words is less than or equal to 1")
  expect_error(ParseWords(x, 6, 1, "word"), "name_field is not a character vector")
  expect_error(ParseWords(x, 6, "text", 1), "var_name is not a character vector")
})

test_that("Words actual seperate into new columns with proper names", {
  expect_equal(ParseWords(x, 6, "text", "word")$text, text)
  expect_equal(ParseWords(x, 6, "text", "word")$word1, c("this", "I", "my", "and"))
  expect_equal(ParseWords(x, 6, "text", "word")$word2, c("is", "love", "name", "my"))
  expect_equal(ParseWords(x, 6, "text", "word")$word3, c("a", "my", "is", "name"))
  expect_equal(ParseWords(x, 6, "text", "word")$word4, c("test", "mom", "Daniel", "is"))
  expect_equal(ParseWords(x, 6, "text", "word")$word5, c(NA, NA, "Tiger", "Prince"))
  expect_equal(ParseWords(x, 6, "text", "word")$word6, c(NA, NA, NA, "Wednesday"))
  expect_equal(ParseWords(x, 3, "text", "whatever") %>% names(), c("text", "whatever1", "whatever2", "whatever3"))
  expect_equal(ParseWords(x, 4, "text", "word")$word4, c("test", "mom", "Daniel Tiger", "is Prince Wednesday"))
})


