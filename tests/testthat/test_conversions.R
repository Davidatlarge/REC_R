context("conversion functions")

test_that("make_column_vector() makes a 1D matrix with n rows", {
  vec <- 1:10
  expect_is(make_column_vector(vec), class = "matrix")
  expect_equal(dim(make_column_vector(vec)), c(length(vec), 1))
})

test_that("make_row_vector() makes a 1D matrix with n colums", {
  vec <- 1:10
  expect_is(make_row_vector(vec), class = "matrix")
  expect_equal(dim(make_row_vector(vec)), c(1, length(vec)))
})

