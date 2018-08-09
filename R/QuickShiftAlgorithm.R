# =============================================================================.
#' QuickShift algorithm (Vedaldi & Soatto, 2008)
# -----------------------------------------------------------------------------.
#' @references
#' Vedaldi A., Soatto S. (2008) Quick Shift and Kernel Methods for Mode Seeking.
#' In: Forsyth D., Torr P., Zisserman A. (eds) Computer Vision – ECCV 2008.
#' ECCV 2008. Lecture Notes in Computer Science, vol 5305.
#' Springer, Berlin, Heidelberg
#' \url{http://dx.doi.org/10.1007/978-3-540-88693-8_52}
# -----------------------------------------------------------------------------.
#' @seealso
#'   \link{QuickShiftClusters},
#'   \link{QuickShift}
# -----------------------------------------------------------------------------.
#' @param x
#' numeric matrix representing multivariate data where rows = observations
#' and columns = measurement conditions.
#'
#' @param d
#' numeric vector representing a density estimation at each observation.
#'
#' @param k
#' number of nearest neighbors used to find increasing densities.
#' The minimum value of is 2 (default).
#'
#' @param plot
#' logical value allowing to visualize the contruction of the QuickShift graph.
#'
#' @param fastest
#' logical value enabling to use a specific and faster implementation
#' when argument \code{k} is equal to 2 (default = TRUE, yes).
#'
#' @param nn_alg
#' nearest neighbor searching algorithm (default = "kd_tree").
#' See \link{get.knn} for further explanations.
#'
#' @return
#' \code{QuickShiftAlgorithm} returns a graph object
#' (see \link{igraph} package). The \link{QuickShiftClusters} function
#' can extract clusters from such graph.
# -----------------------------------------------------------------------------.
#' @export
QuickShiftAlgorithm <- function (
  x, d, k = 2, plot = FALSE, fastest = TRUE, nn_alg = "kd_tree"
) {

  display_iteration <- function() {
    xyl <- range(x)
    plot.default(NA, xlim = xyl, ylim = xyl, axes = F, xlab = "", ylab = "")
    PlotQuickShift(x, g, new = F, length = 0.1, lwd = 1.5)
    # text(x, labels = d, pos = 1, cex = 0.8)
  }

  # Initialization
  g <- igraph::graph.empty(n = nrow(x))
  i.a <- which(FiniteValues(x) & ! is.na(d))

  if(k == 2 & fastest) {
    # Fastest implementation ---------------------------------------------------
    while(length(i.a) >= 2) {

      knn <- FNN::get.knnx(
        data = x[i.a, ], query = x[i.a, ], k = 2, algorithm = nn_alg
      )

      i.b <- i.a[knn$nn.index[, 2]]
      chk <- d[i.b] >= d[i.a]

      g <- g + igraph::edges(
        rbind(i.a, i.b)[, chk], distance = knn$nn.dist[chk, 2]
      )
      i.a <- i.a[! chk]

      if(plot) display_iteration()
    }
  } else {
    # General implementation ---------------------------------------------------
    n <- Inf
    while(length(i.a) >= 2 & length(i.a) < n) {

      n <- length(i.a)
      w <- min(k, n)

      knn <- FNN::get.knnx(
        data = x[i.a, ], query = x[i.a, ], k = w, algorithm = nn_alg
      )

      i <- knn_values(i.a, knn$nn.index)
      j <- apply(knn_values(d, i)[, 2:w, drop = FALSE], 1, which.max) + 1
      j <- m2v(1:n, j, nrow = n)
      idx <- knn$nn.index[j]
      dis <- knn$nn.dist[j]

      i.b <- i.a[idx]
      chk <- d[i.b] >= d[i.a]
      g <- g + igraph::edges(
        rbind(i.a, i.b)[, chk] , distance = dis[chk]
      )
      i.a <- i.a[! chk]

      if(plot) display_iteration()
    }
  }

  g
}
