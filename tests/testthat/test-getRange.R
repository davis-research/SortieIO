context("getRange")

test_that("getRange outputs correctly", {
  expect_equal(getRange(c(12,2,4)), 10)
  expect_error(getRange("a"), "x")
})
