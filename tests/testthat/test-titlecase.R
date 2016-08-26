context("TitleCase function")

test_that("TitleCase works", {
  x <- c("mike smith", "MaRy lOwE", "Jose McDonald", "MATT MACRO")
  expect_equal(TitleCase(x[1]), "Mike Smith")
  expect_equal(TitleCase(x[2]), "Mary Lowe")
  expect_equal(TitleCase(x[3]), "Jose Mcdonald")
  expect_equal(TitleCase(x[4]), "Matt Macro")
  expect_warning(TitleCase(x), "Length is greater than 1, only first element will be returned.")
  expect_error(TitleCase(1), "v is not a character vector")
})
