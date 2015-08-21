context("TryDTest")

test_that("TryDTest outputs correctly", {
  expect_equal(TryDTest(c(1:10, 45))<0.05, T)
  expect_equal(TryDTest(1),1)
})
