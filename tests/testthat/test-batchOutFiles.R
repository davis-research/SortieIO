context("batchOutFile")

test_that("batchOutFiles functions", {
  expect_equal(batchOutFiles("out"), testOutImport)
})

test_that("batchOutFiles type checks correctly", {
  expect_error(batchOutFiles(1), "namepattern")
  expect_error(batchOutFiles("out", yearoffset="candy"), "yearoffset")
  expect_error(batchOutFiles("rawr"), "No files")
})
