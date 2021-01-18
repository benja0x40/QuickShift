# > counts =====================================================================
context("Dithering")

# + Dithering ---------------------------------------------------------------
test_that("Basics", {
  # Test ////
  x <- rep(1, 1000)
  r <- Dithering(x)
  d <- abs(x - r)
  expect_false(any(duplicated(r)))
  expect_true(max(d) < 0.5)
  expect_equal(round(min(r), 1), min(x) - 0.5)
  expect_equal(round(max(r), 1), max(x) + 0.5)
  # Test ////
  x <- rep(5, 1000)
  r <- Dithering(x)
  d <- abs(x - r)
  expect_false(any(duplicated(r)))
  expect_true(max(d) < 0.5)
  expect_equal(round(min(r), 1), min(x) - 0.5)
  expect_equal(round(max(r), 1), max(x) + 0.5)
  # Test ////
  x <- rep(1:5, 1000)
  r <- Dithering(x)
  d <- abs(x - r)
  expect_false(any(duplicated(r)))
  expect_true(max(d) < 1.0)
  expect_equal(round(min(r), 1), min(x) - 0.5)
  expect_equal(round(max(r), 1), max(x) + 0.5)
  # Test ////
  x <- rep(5:9, 1000)
  r <- Dithering(x)
  d <- abs(x - r)
  expect_false(any(duplicated(r)))
  expect_true(max(d) < 1.0)
  expect_equal(round(min(r), 1), min(x) - 0.5)
  expect_equal(round(max(r), 1), max(x) + 0.5)
  # Test ////
  x <- rep(c(0, 1:5), 1000)
  k <- x == 0
  r <- Dithering(x)
  d <- abs(x - r)
  expect_false(any(duplicated(r[!k])))
  expect_true(max(d) < 1.0)
  expect_true(all(r[k] == 0))
  expect_equal(round(min(r[!k]), 1), min(x[!k]) - 0.5)
  expect_equal(round(max(r[!k]), 1), max(x[!k]) + 0.5)
  # Test ////
  x <- rep(c(0, 5:9), 1000)
  k <- x == 0
  r <- Dithering(x)
  d <- abs(x - r)
  expect_false(any(duplicated(r[!k])))
  expect_true(max(d) < 1.0)
  expect_true(all(r[k] == 0))
  expect_equal(round(min(r[!k]), 1), min(x[!k]) - 0.5)
  expect_equal(round(max(r[!k]), 1), max(x[!k]) + 0.5)
})
