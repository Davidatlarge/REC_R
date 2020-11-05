context("conversion functions")

test_that("make_column_vector() makes a 1D matrix", {
  vec <- 1:10
  expect_is(make_column_vector(vec), class = "matrix")
  expect_equal(dim(make_column_vector(vec)), c(length(vec), 1))
})

