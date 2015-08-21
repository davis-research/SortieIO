context("changeColsBySpp")

testChangeDf <- data.frame(AbsBA.X=1:4, AbsBA.Y=1:4)
testExpDf <- data.frame(key=c(rep("X", 4), rep("Y", 4)), val=1:4, stringsAsFactors=F)


test_that("changeColsBySpp works", {
  expect_equal(changeColsBySpp(testChangeDf, "AbsBA."), testExpDf)
})

test_that("changeColsBySpp checks types correctly", {
  expect_error(changeColsBySpp("rawr"), "df")
  expect_error(changeColsBySpp(testChangeDf, removalkey=1), "removalkey")
  expect_error(changeColsBySpp(testChangeDf, "AbsBa.", "rawr"), "dead")
})

rm(list=c("testChangeDf", "testExpDf"))
