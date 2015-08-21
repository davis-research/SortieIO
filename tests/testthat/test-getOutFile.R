context("getOutFile")


cat("a", "b", "c", "d", "e", "Col1\tCol2", "text1\ttext2",
    file="ex.data",
    sep="\n")
cat("a", "b", "c", "d", "e", "Col1\tCol2", "text1\ttext2",
    file="ex2.data",
    sep="\n")

test_that("getOutFile imports single file correctly", {
  expect_equal(getOutFile("ex.data"), data.frame(Col1="text1", Col2="text2", stringsAsFactors=F) )
})
test_that("getOutFile imports several files correctly", {
  expect_equal(getOutFile(c("ex.data", "ex2.data")), data.frame(Col1=rep("text1",2), Col2=rep("text2",2), stringsAsFactors=F))
})

unlink("ex.data")
unlink("ex2.data")
