# > counts =====================================================================
context("MatchClusters")

# + MatchClusters --------------------------------------------------------------
test_that("Basic", {
  r <- c(1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3)

  x <- c(2, 3, 3, 3, 3, 2, 2, 3, 3, 3, 1, 1, 1, 2, 3)
  i <- MatchClusters(x, r)
  expect_identical(i[, 1], as.numeric(3:1))
  expect_identical(i[, 2], as.numeric(3:1))

  # x <- c(2, 3, 3, 3, 3, 1, 1, 1, 2, 2, 1, 1, 2, 2, 3)
  # i <- MatchClusters(x, r, n = 10)
  # expect_identical(i[, 1], c(2, 3, 1, 4:10))
  # expect_identical(i[, 2], c(3, 1, 2, 4:10))
})
