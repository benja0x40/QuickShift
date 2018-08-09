# DEPRECATED ###################################################################

# =============================================================================.
#
# -----------------------------------------------------------------------------.
translation_2D <- function(v, dv) {
  v + matrix(dv, nrow(v), length(dv), byrow = TRUE)
}
# =============================================================================.
#
# -----------------------------------------------------------------------------.
make_square_grid_2D <- function(n, normalized = TRUE) {
  x <- rep(1:n, n) - (n + 1) / 2
  y <- rep(1:n, each = n) - (n + 1) / 2
  xy <- cbind(x, y)
  if(normalized) xy <- 2 / n * xy
  xy
}
make_disk_grid_2D <- function(n, normalized = TRUE) {
  x <- rep(1:n, n) - (n + 1) / 2
  y <- rep(1:n, each = n) - (n + 1) / 2
  d <- sqrt(x^2 + y^2)
  xy <- cbind(x, y)[d < n / 2, ]
  if(normalized) xy <- 2 / n * xy
  xy
}
make_ring_grid_2D <- function(n, normalized = TRUE) {
  x <- rep(1:n, n) - (n + 1) / 2
  y <- rep(1:n, each = n) - (n + 1) / 2
  d <- sqrt(x^2 + y^2)
  xy <- cbind(x, y)[d < n / 2 & d > n / 4, ]
  if(normalized) xy <- 2 / n * xy
  xy
}
# =============================================================================.
#
# -----------------------------------------------------------------------------.
make_square_unif_2D <- function(n, normalized = TRUE) {
  x <- n * (runif(n * n) - 1 / 2)
  y <- n * (runif(n * n) - 1 / 2)
  xy <- cbind(x, y)
  if(normalized) xy <- 2 / n * xy
  xy
}
make_disk_unif_2D <- function(n, normalized = TRUE) {
  x <- n * (runif(n * n) - 1 / 2)
  y <- n * (runif(n * n) - 1 / 2)
  d <- sqrt(x^2 + y^2)
  xy <- cbind(x, y)[d < n / 2, ]
  if(normalized) xy <- 2 / n * xy
  xy
}
make_ring_unif_2D <- function(n, normalized = TRUE) {
  x <- n * (runif(n * n) - 1 / 2)
  y <- n * (runif(n * n) - 1 / 2)
  d <- sqrt(x^2 + y^2)
  xy <- cbind(x, y)[d < n / 2 & d > n / 4, ]
  if(normalized) xy <- 2 / n * xy
  xy
}

# FUNCTIONS ####################################################################

# =============================================================================.
#
# -----------------------------------------------------------------------------.
ShowClusters <- function(M, xyl, q = NULL) {
  s <- c(21, 19, 22, 15, 23, 8)
  s <- c(21, 19, 8)
  if(is.null(q$membership)) s <- s[2]
  else s <- s[q$membership]
  plot.default(NA, xlim = xyl, ylim = xyl, axes = F, xlab = "", ylab = "")
  points(M, pch = s, col = grey(0, 0.5))
}
# =============================================================================.
#
# -----------------------------------------------------------------------------.
CombineTestObjects <- function(...) {
  tst <- list(...)
  tst <- list(
    M   = do.call(rbind, sapply(tst, "[", "M")),
    d   = as.vector(unlist(sapply(tst, "[", "d"))),
    xyl = do.call(range, sapply(tst, "[", "xyl"))
  )
  tst
}
# =============================================================================.
#
# -----------------------------------------------------------------------------.
Circular2D <- function(n, r, s) {
  a <- 0:(n - 1) / (n - 1) * pi
  a <- rnorm(n) / 2
  r <- r + rnorm(n, sd = s)
  x <- r * cos(a)
  y <- r * sin(a)
  cbind(x, y)
}
# =============================================================================.
#
# -----------------------------------------------------------------------------.
TestObject <- function(i, n, r = 1) {
  q <- 0:(n - 1) / (n - 1)
  if(i == 1) {
    M <- cbind(
      x = 2 * r * (c(q, rev(q)) - 1 / 2),
      y = r * c(rep(1, n), rep(-1, n))
    )
    d <- c(1:n, 1 + 1:n)
    xyl <- 1.1 * c(-r, r)
  }
  if(i == 2) {
    a <- 2:(n - 1) / n * 2 * pi
    M <- cbind(
      x = - r * cos(a),
      y =   r * sin(a)
    )
    d <- 2:(n - 1)
    xyl <- 1.1 * c(-r, r)
  }
  if(i == 3) {
    M <- rbind(
      cbind(rnorm(n), rnorm(n)),
      Circular2D(10 * n,  r, 1)
    )
    d <- NULL
    xyl <- range(M)
  }
  list(M = M, d = d, xyl = xyl)
}

# =============================================================================.
#
# -----------------------------------------------------------------------------.
# n <- 100
# layout(matrix(1:9, 3, 3, byrow = TRUE))
# plot(make_square_grid_2D(n), pch = 20, col = grey(0, alpha = 0.5))
# plot(make_disk_grid_2D(n), pch = 20, col = grey(0, alpha = 0.5))
# plot(make_ring_grid_2D(n), pch = 20, col = grey(0, alpha = 0.5))
# plot(make_square_unif_2D(n), pch = 20, col = grey(0, alpha = 0.5))
# plot(make_disk_unif_2D(n), pch = 20, col = grey(0, alpha = 0.5))
# plot(make_ring_unif_2D(n), pch = 20, col = grey(0, alpha = 0.5))

# TESTS ########################################################################

# > QuickShift =================================================================
context("QuickShift")

# + Careful -----------------------------------------------------------------
test_that("Careful", {

  for(f in c(TRUE, FALSE)) {
    tst <- TestObject(1, n = 5)
    q <- with(
      tst, QuickShift(
        M, n = 2, d = d, q = 2, fastest = f, graph = TRUE, plot = TRUE
      )
    )

    expect_identical(q$nclust, 2)
    expect_equal(as.vector(q$csizes), c(5, 5))

    expect_identical(q$membership, rep(1:2, each = 5))

    e <- as_edgelist(q$graph)
    e <- e[order(e[, 1]),]

    expect_equal(e[, 1], c(1:4, 6:9))
    expect_equal(e[, 2], c(2:5, 7:8, 10, 10))

  }

  tst <- CombineTestObjects(TestObject(1, n = 5), TestObject(2, n = 18, r = 4))

  expect_identical(
    with(tst, QuickShift(M, n = 3, d = d, q = 2, fastest = TRUE)),
    with(tst, QuickShift(M, n = 3, d = d, q = 2, fastest = FALSE))
  )

  layout(matrix(1:9, 3, 3, byrow = T))
  q <- with(tst, QuickShift(M, n = 3, d = d, q = 3, graph = TRUE, plot = TRUE))
  PlotQuickShift(tst$M, q$graph)

  expect_identical(q$nclust, 3)
  expect_equal(as.vector(q$csizes), c(16, 5, 5))

  expect_equal(q$membership, c(rep(3, 5), rep(1, 5), rep(2, 16)))

  layout(matrix(1:9, 3, 3, byrow = T))
  tst <- TestObject(3, n = 100, r = 100)
  q <- with(tst, QuickShift(M, n = 2, q = 2, decreasing = FALSE, plot = TRUE))
  with(tst, ShowClusters(M, xyl, q))

  expect_identical(q$nclust, 2)
  expect_equal(as.vector(q$csizes), c(100, 1000))
  expect_equal(q$membership, c(rep(1, 100), rep(2, 1000)))

  g <- igraph::make_ring(10)
  igraph::V(g)$id <- 1:10
  igraph::E(g)$distance <- 1
  expect_warning(QuickShiftClusters(g, 2))

  # Cleanup
  file.remove("Rplots.pdf")
})

# + Basics -----------------------------------------------------------------
# test_that("Basics", {
#
#   layout(matrix(1:9, 3, 3, byrow = TRUE))
#
#   # Test ////
#   n <- 30
#   x <- rbind(
#     translation_2D(make_square_unif_2D(n / 3), c( 0,  0)),
#     translation_2D(make_square_unif_2D(n / 2), c(-3, -3)),
#     translation_2D(make_square_unif_2D(n / 1), c( 4,  4))
#   )
#   mbr <- c(rep(3, (n / 3)^2), rep(2, (n / 2)^2), rep(1, (n / 1)^2))
#   nbr <- (n / (1:3))^2
#
#   d <- knn_density(x, k = 50)
#   # plot(x, pch = 20, col = grey(0, alpha = 0.5))
#   # plot(x, pch = 20, col = colorize(d))
#   g <- QuickShiftAlgorithm(x, d)
#   grp <- QuickShiftClusters(g, n = 3)
#   # clr <- rainbow(grp$nbr, alpha = 0.5)
#   # clr <- plot_groups_2D(x, clr = clr[grp$membership], pch = 20)
#
#   expect_equal(as.vector(grp$csizes), nbr)
#   expect_true(all(grp$membership == mbr))
#
#   # Test ////
#   n <- 20
#   x <- rbind(
#     translation_2D(make_square_unif_2D(n) / 1, c( 0,  0)),
#     translation_2D(make_square_unif_2D(n) / 2, c(-2, -2)),
#     translation_2D(make_square_unif_2D(n) / 3, c( 3,  3))
#   )
#   mbr <- rep(1:3, each = n^2)
#   nbr <- rep(n^2, 3)
#
#   d <- knn_density(x, k = 50)
#   # plot(x, pch = 20, col = grey(0, alpha = 0.5))
#   # plot(x, pch = 20, col = colorize(d))
#   g <- QuickShiftAlgorithm(x, d, k = 3)
#   grp <- QuickShiftClusters(g, n = 3)
#   # clr <- rainbow(grp$nbr, alpha = 0.5)
#   # clr <- plot_groups_2D(x, clr = clr[grp$membership], pch = 20)
#
#   expect_equal(as.vector(grp$csizes), nbr)
#   expect_true(all(grp$membership == mbr))
#
# })
