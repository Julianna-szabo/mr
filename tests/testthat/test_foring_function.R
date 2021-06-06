context('forint_test')

library(mr)
library(stats)

test_that("Number is coverted into a string and followed by Ft", {
  expect_equal( forint(42), "42 Ft")
})
#> Test passed 😀

test_that("Integer is converted to a string with additional decimal 0 and Ft is added to the end", {
  expect_equal( forint(42.5), "42.50 Ft")
})
#> Test passed 😀

test_that("Integer is converted to a string with decimal and followed by Ft", {
  expect_equal( forint(42.59), "42.59 Ft")
})
#> Test passed 😀