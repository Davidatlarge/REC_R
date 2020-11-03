context("derivatives functions")

test_that("abl_1 fails with missing argumemts.", {
  expect_error(abl_1())
  expect_error(abl_1(f = 1:10))
  expect_error(abl_1(s = 1:10))
})

test_that("abl_1 fails with non-numeric input.", {
  expect_error(abl_1(f = "a", s = "b"))
  expect_error(abl_1(f = 1:10, s = "b"))
  expect_error(abl_1(f = "a", s = 1:10))
})

test_that("abl_1 computes the first derivative.", {
  x <- 1:100
  expect_equal(abl_1(f = (x)^2, s = x), 2 * (x))
})
