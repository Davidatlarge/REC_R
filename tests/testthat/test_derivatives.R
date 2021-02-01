context("derivatives functions")

source("../../abl_1.R")
source("../../abl_1_non_aequi.R")

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
  expect_equal(abl_1(f = x^2, s = x), 2 * x)
  #expect_equal(abl_1(f = x^3, s = x), 3 * x^2)
})

test_that("abl_1_non_aequi fails with missing argumemts.", {
  expect_error(abl_1_non_aequi())
  expect_error(abl_1_non_aequi(f = 1:10))
  expect_error(abl_1_non_aequi(s = 1:10))
})

test_that("abl_1_non_aequi fails with non-numeric input.", {
  expect_error(abl_1_non_aequi(f = "a", s = "b"))
  expect_error(abl_1_non_aequi(f = 1:10, s = "b"))
  expect_error(abl_1_non_aequi(f = "a", s = 1:10))
})

test_that("abl_1_non_aequi computes the first derivative on a non equidistant grid.", {
  x <- sample(1:100, 80) %>% sort()
  expect_equal(abl_1_non_aequi(f = (x)^2, s = x), 2 * (x))
  #expect_equal(abl_1_non_aequi(f = x^3, s = x), 3 * x^2)
})
