context("clean_email function")

test_that("invalid characters are removed", {
  expect_equal(clean_email("N/A@gmail.com"), "@GMAIL.COM")
  expect_equal(clean_email("noemail"), "")
  expect_equal(clean_email("noemail@FAKe.com"), "COM")
  expect_equal(clean_email("noemail@FAKe."), "")
  expect_equal(clean_email("  bronyforlife@hotmail.com   "), toupper("BRONYFORLIFE@HOTMAIL.COM"))
})
