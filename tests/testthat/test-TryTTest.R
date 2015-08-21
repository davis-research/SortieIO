context("TryTTest")

test_that("TryTTest works", {
  expect_equal(TryTTest(1), 1)
  expect_equal(TryTTest(1:10, mu=45)<0.05, T)
})
