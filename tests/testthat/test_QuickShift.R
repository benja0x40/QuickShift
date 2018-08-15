# FUNCTIONS ####################################################################

# =============================================================================.
#
# -----------------------------------------------------------------------------.
ShowClusters <- function(M, xyl, q = NULL, use_colors = FALSE) {
  s <- c(21, 19, 22, 15, 23, 8)
  s <- c(21, 19, 8)
  clr <- grey(0, 0.5)
  if(is.null(q$membership)) {
    s <- s[2]
  } else {
    s <- s[q$membership]
    if(use_colors) clr <- rainbow(q$nclust)[q$membership]
  }
  plot.default(NA, xlim = xyl, ylim = xyl, axes = F, xlab = "", ylab = "")
  points(M, pch = s, col = clr)
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
    d <- c(1:n, 1:n)
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

  tst <- TestObject(1, n = 5)
  q <- with(tst, QuickShift(M, n = 2, d = d, graph = TRUE, plot = TRUE))

  expect_identical(q$nclust, 2)
  expect_equal(as.vector(q$csizes), c(5, 5))

  expect_identical(q$membership, rep(1:2, each = 5))

  e <- as_edgelist(q$graph)
  e <- e[order(e[, 1]),]

  expect_equal(e[, 1], c(1:4, 6:9))
  expect_equal(e[, 2], c(2:5, 7:10))

  tst <- CombineTestObjects(TestObject(1, n = 5), TestObject(2, n = 18, r = 4))

  layout(matrix(1:9, 3, 3, byrow = T))
  q <- with(tst, QuickShift(M, n = 3, d = d, graph = TRUE, plot = TRUE))
  PlotQuickShift(tst$M, q$graph)

  expect_identical(q$nclust, 3)
  expect_equal(as.vector(q$csizes), c(16, 5, 5))

  expect_equal(q$membership, c(rep(3, 5), rep(1, 5), rep(2, 16)))

  layout(matrix(1:9, 3, 3, byrow = T))
  tst <- TestObject(3, n = 100, r = 100)
  q <- with(tst, QuickShift(M, n = 2, decreasing = FALSE))
  with(tst, ShowClusters(M, xyl, q, use_colors = TRUE))

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
