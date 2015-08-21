context("reNumber")

test_that("reNumber gives correct output", {
  expect_equal(reNumber(data.frame(x=1:4, y=5:8)[-1,]), data.frame(x=2:4, y=6:8))
})

test_that("reNumber type checks", {
  expect_error(reNumber("rawr"), "x")
})
