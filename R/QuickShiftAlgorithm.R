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
#' @description
#' Implementation of the QuickShift algorithm using accelerated nearest
#' neighbor queries from the FNN package.
#'
#' @param x
#' numeric matrix representing multivariate data where rows = observations
#' and columns = measurement conditions.
#'
#' @param d
#' numeric vector representing a density estimation at each observation.
#'
#' @param plot
#' logical value allowing to visualize the contruction of the QuickShift graph.
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
QuickShiftAlgorithm <- function (x, d, plot = FALSE, nn_alg = "kd_tree") {

  display_iteration <- function() {
    xyl <- range(x)
    plot.default(NA, xlim = xyl, ylim = xyl, axes = F, xlab = "", ylab = "")
    PlotQuickShift(x, g, new = F, length = 0.1, lwd = 1.5)
    # text(x, labels = d, pos = 1, cex = 0.8)
  }

  # Initialization
  g <- igraph::graph.empty(n = nrow(x))
  i.a <- which(FiniteValues(x) & ! is.na(d))

  k_s <- 2
  k_e <- 4
  nbr <- 0

  while(length(i.a) >= 2) {

    knn <- FNN::get.knnx(
      data = x, query = x[i.a, ], k = k_e, algorithm = nn_alg
    )

    msk <- rep(FALSE, length(i.a))
    for(j in k_s:k_e) {

      i.b <- knn$nn.index[, j] # Nearest neighbor of rank j
      tst <- d[i.b] >= d[i.a]  # Density test
      chk <- tst & ! msk       # Avoid to connect already connected points

      # Connect data points to nearest neighbor with increased density
      if(any(chk)) {
        g <- g + igraph::edges(
          rbind(i.a, i.b)[, chk], distance = knn$nn.dist[chk, j]
        )
      }

      # Keep track of already connected data points
      msk <- msk | tst
      nbr <- nbr + sum(chk)

      # Stop when all data points are connected
      if(nbr >= nrow(x)) break
    }
    i.a <- i.a[! msk] # Drop already connected data points

    # Update the rank of nearest neighbors to be scanned in next iteration
    k_s <- k_e + 1
    k_e <- min(nrow(x), 2 * k_e)

    if(plot) display_iteration()
  }

  g
}
