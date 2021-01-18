# > Other Functions ============================================================
context("Other Functions")

# + FiniteValues ---------------------------------------------------------------
test_that("FiniteValues", {
  tst <- log(c(0, NA, 1, Inf))
  res <- c(FALSE, FALSE, TRUE, FALSE)
  expect_equal(FiniteValues(tst), res)
  expect_equal(FiniteValues(cbind(1, tst)), res)
  expect_equal(FiniteValues(cbind(tst, 1)), res)
  expect_equal(FiniteValues(cbind(1, tst, 1)), res)
  expect_equal(FiniteValues(cbind(tst, tst)), res)
  expect_equal(FiniteValues(matrix(tst, 4, 1)), res)
  expect_equal(FiniteValues(matrix(tst, 4, 4)), res)
})

# + RankScore -------------------------------------------------------------------
test_that("RankScore", {
  expect_equal(RankScore(1), 0.5)
  expect_equal(RankScore(1:2), c(0.25, 0.75))
  expect_equal(RankScore(2:1), c(0.75, 0.25))
  expect_equal(range(RankScore(1:10)), c(0.05, 0.95))

  m <- r <- matrix(1, 5, 2, dimnames = list(NULL, c("x", "y")))
  r[] <- 0.5
  expect_identical(RankScore(m), r)
})

# + S01 ------------------------------------------------------------------------
test_that("S01", {
  expect_equal(range(S01(0:10)), c(0, 1))
  expect_equal(range(S01(-5:5)), c(0, 1))
})

# + SX2Y -----------------------------------------------------------------------
test_that("SX2Y", {
  a <- 0:10
  b <- -5:5
  expect_equal(range(SX2Y(a, b)), range(b))
  expect_equal(range(SX2Y(b, a)), range(a))
})

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

# + m2v ------------------------------------------------------------------------
test_that("m2v", {
  a <- m2v(1:3, 1:3, nrow = 3)
  b <- c(1, 5, 9)
  expect_equal(a, b)
  a <- m2v(3:1, 1:3, nrow = 3)
  b <- c(3, 5, 7)
  expect_equal(a, b)
})

# + v2m ------------------------------------------------------------------------
test_that("v2m", {
  a <- v2m(c(1, 5, 9), nrow = 3)
  b <- cbind(1:3, 1:3)
  expect_equal(a, b)
  a <- v2m(c(3, 5, 7), nrow = 3)
  b <- cbind(3:1, 1:3)
  expect_equal(a, b)
})
