# > knn_density ================================================================
context("knn_density")

# + density 1D -----------------------------------------------------------------
test_that("density 1D", {

  n1 <- 10000
  n2 <- 20000
  g <- c(rep(1, n1), rep(2, n2))
  x <- c(rnorm(n1, 0, 1), rnorm(n2, 0, 5))

  o <- order(x)
  x <- x[o]
  g <- g[o]

  dt <- knn_density(x, k = 50)
  d1 <- knn_density(x, xref = x[g == 1], k = 50)
  d2 <- knn_density(x, xref = x[g == 2], k = 50)

  n <- length(x)
  expect_identical(length(dt), n)
  expect_identical(length(d1), n)
  expect_identical(length(d2), n)

  expect_true(all(dt >= 0))
  expect_true(all(d1 >= 0))
  expect_true(all(d2 >= 0))

  expect_true(mean(abs(d1 + d2 - dt)) < 1E-2)

  p <- dnorm(x[g == 1], 0, 1)
  chk <- sum(abs(p - 3 * d1[g == 1])) / n1
  # plot(x[g == 1], 3 * d1[g == 1], col = "red")
  # points(x[g == 1], p)
  expect_true(chk < 0.05)

  p <- dnorm(x[g == 2], 0, 5)
  chk <- sum(abs(p - 3/2 * d2[g == 2])) / n2
  # plot(x[g == 2], 3/2 * d2[g == 2], col = "red")
  # points(x[g == 2], p)
  expect_true(chk < 0.05)

})

# + density 2D -----------------------------------------------------------------
test_that("density 2D", {

  n1 <- 10000
  n2 <- 40000
  g <- c(rep(1, n1), rep(2, n2))
  V <- cbind(
    c(rnorm(n1, 0, 1), rnorm(n2, 0, 5)),
    c(rnorm(n1, 0, 1), rnorm(n2, 0, 5))
  )

  dt <- knn_density(V, k = 50)
  d1 <- knn_density(V, xref = V[g == 1, ], k = 50)
  d2 <- knn_density(V, xref = V[g == 2, ], k = 50)

  n <- dim(V)
  expect_identical(length(dt), n[1])
  expect_identical(length(d1), n[1])
  expect_identical(length(d2), n[1])

  expect_true(all(dt >= 0))
  expect_true(all(d1 >= 0))
  expect_true(all(d2 >= 0))

  expect_true(mean(abs(d1 + d2 - dt)) < 1E-2)

})

# > knn_stats ==================================================================
context("knn_stats")

n <- 1000

mu    <- c(-30, -10, 10, 30)
sigma <- c(3, 1, 1, 3)

u <- cbind(mu, mu)
v <- cbind(sigma, sigma)

x <- rnorm(n, mean = mu, sd = sigma)
y <- rnorm(n, mean = mu, sd = sigma)

M <- cbind(x, y)

plot(M, col = grey(0, 0.1))
points(u, pch = 20, col = "red")

# + knn_musigma2 ---------------------------------------------------------------
test_that("knn_musigma2", {

  r <- knn_musigma2(M, k = 100)

  chk <- matrix(u, nrow = nrow(M), ncol = ncol(M))
  chk <- sum(abs(chk - r$mu)) / length(chk)
  expect_true(all(chk < 1.0))

  chk <- matrix(v, nrow = nrow(M), ncol = ncol(M))
  chk <- sum(abs(chk - sqrt(r$sigma2))) / length(chk)
  expect_true(all(chk < 1.0))

  i <- seq(1, nrow(M), 2)
  r <- knn_musigma2(M[i, ], xref = M, k = 100, smoothing = TRUE)

  chk <- matrix(u, nrow = nrow(M), ncol = ncol(M))[i, ]
  chk <- sum(abs(chk - r$mu)) / length(chk)
  expect_true(all(chk < 1.0))

  chk <- matrix(v, nrow = nrow(M), ncol = ncol(M))[i, ]
  chk <- sum(abs(chk - sqrt(r$sigma2))) / length(chk)
  expect_true(all(chk < 1.0))
})

# + knn_mean -------------------------------------------------------------------
test_that("knn_mean", {
  r <- knn_mean(M, k = 100)

  chk <- matrix(u, nrow = nrow(M), ncol = ncol(M))
  chk <- sum(abs(chk - r)) / length(chk)
  expect_true(all(chk < 1.0))

  i <- seq(1, nrow(M), 2)
  r <- knn_mean(M[i, ], xref = M, k = 100)

  chk <- matrix(u, nrow = nrow(M), ncol = ncol(M))[i, ]
  chk <- sum(abs(chk - r)) / length(chk)
  expect_true(all(chk < 1.0))
})
