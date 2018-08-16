# > Arguments ==================================================================
context("Arguments")

# + VectorArgs -----------------------------------------------------------------
test_that("VectorArgs", {

  x <- 0
  y <- 1:10

  VectorArgs(c("x", "y"))
  expect_identical(x, rep(0, 10))
  expect_identical(y, 1:10)

  VectorArgs(c("x", "y"), size = 15)
  expect_identical(x, rep(0, 15))
  expect_identical(y[11:15], 1:5)

  a <- list(x = 0, y = 1:10)

  r <- VectorArgs(c("x", "y"), from = a)
  expect_identical(r$x, rep(0, 10))
  expect_identical(r$y, 1:10)

  r <- VectorArgs(c("x", "y"), from = a, size = 15)
  expect_identical(r$x, rep(0, 15))
  expect_identical(r$y[11:15], 1:5)

})
