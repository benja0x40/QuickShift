# > knn_stats ==================================================================
context("knn_stats")

n <- 10000

mu    <- c(-15, -5, 5, 15)
sigma <- c(2, 1, 1, 2)

u <- cbind(mu, mu)
v <- cbind(sigma, sigma)

x <- rnorm(n, mean = mu, sd = sigma)
y <- rnorm(n, mean = mu, sd = sigma)

M <- cbind(x, y)

# plot(M, col = grey(0, 0.1))
# points(u, pch = 20, col = "red")

# + knn_density ----------------------------------------------------------------
test_that("knn_density", {

  n <- 10000
  z <- sort(rnorm(n))

  p <- dnorm(z)
  d <- knn_density(z, k = 256)
  chk <- z > -2 & z < 2
  chk <- sum(abs(p - d)[chk]) / sum(chk)
  expect_true(chk < 0.05)
  # plot(z, d, type = 'l', ylim = c(0, 0.5))
  # points(z, p, type = 'l', col = "red")

  d <- knn_density(M, k = 128)
  expect_true(all(d >= 0))
})

# + knn_musigma2 ---------------------------------------------------------------
test_that("knn_musigma2", {

  r <- knn_musigma2(M, k = 256)

  chk <- matrix(u, nrow = nrow(M), ncol = ncol(M))
  chk <- sum(abs(chk - r$mu)) / length(chk)
  expect_true(all(chk < 1.0))

  chk <- matrix(v, nrow = nrow(M), ncol = ncol(M))
  chk <- sum(abs(chk - sqrt(r$sigma2))) / length(chk)
  expect_true(all(chk < 1.0))
})

# + knn_mean -------------------------------------------------------------------
test_that("knn_mean", {
  r <- knn_mean(M, k = 256)

  chk <- matrix(u, nrow = nrow(M), ncol = ncol(M))
  chk <- sum(abs(chk - r)) / length(chk)
  expect_true(all(chk < 1.0))
})
