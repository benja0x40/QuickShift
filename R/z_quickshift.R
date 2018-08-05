# FUNCTIONS | QUICKSHIFT #######################################################

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
#'   \link{QuickShiftClustering}
# -----------------------------------------------------------------------------.
#' @param x
#' numeric matrix representing multivariate data where rows = observations
#' and columns = measurement conditions.
#'
#' @param d
#' numeric vector representing a density estimation at each observation.
#'
#' @return
#' \code{QuickShiftAlgorithm} returns a graph object (see \link{igraph} package).
# -----------------------------------------------------------------------------.
#' @import methods
#' @import grDevices
#' @import graphics
#' @import igraph
#' @importFrom matrixStats colSums2 colMeans2 rowSums2 rowMeans2
#' @importFrom FNN get.knn get.knnx knnx.dist
# -----------------------------------------------------------------------------.
#' @keywords internal
#' @export
QuickShiftAlgorithm <- function (x, d) {

  g <- graph.empty(n = nrow(x))
  i.a <- which(FiniteValues(x) & ! is.na(d))

  while(length(i.a) > 1) {

    knn <- FNN::get.knnx(
      data = x[i.a, ], query = x[i.a, ], k = 2, algorithm = "kd_tree"
    )

    i.b <- i.a[knn$nn.index[, 2]]
    chk <- d[i.b] >= d[i.a]

    g <- g + igraph::edges(
      rbind(i.a, i.b)[, chk], distance = knn$nn.dist[chk, 2]
    )
    i.a <- i.a[! chk]
  }

  g
}
# =============================================================================.
#' QuickShift algorithm (Vedaldi & Soatto, 2008) - alternative implementation
# -----------------------------------------------------------------------------.
#' @keywords internal
#' @export
QuickShiftAlgorithmAlt <- function (x, d, k = 3, plot = FALSE, alg = 0) {

  g <- graph.empty(n = nrow(x))
  i.0 <- i.a <- which(FiniteValues(x) & ! is.na(d))

  n <- Inf
  while(length(i.a) >= 2 & length(i.a) < n) {

    n <- length(i.a)
    w <- min(k, n)

    knn <- FNN::get.knnx(
      data = x[i.a, ], query = x[i.a, ], k = w,
      algorithm = "kd_tree"
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
    if(alg == 0) i.a <- i.a[! chk]
    if(alg == 1) i.a <- unique(c(i.a[! chk], i.b[chk]))

    if(plot) {
      xyl <- range(x)
      plot.default(NA, xlim = xyl, ylim = xyl, axes = F, xlab = "", ylab = "")
      PlotQuickShift(x, g, new = F, length = 0.1, lwd = 1.5)
      text(x, labels = d, pos = 1, cex = 0.8)
    }
  }

  g
}

# =============================================================================.
#' Split a QuickShift graph into clusters
# -----------------------------------------------------------------------------.
#' @seealso
#'   \link{QuickShiftAlgorithm},
#'   \link{QuickShiftClustering}
# -----------------------------------------------------------------------------.
#' @param g
#' QuickShift graph resulting from the \link{QuickShiftAlgorithm} function.
#'
#' @param n
#' desired number of clusters.
#'
#' @return
#' \code{QuickShiftClustering} and \code{QuickShiftClusters} return a list
#' with the same following elements:
#' \item{membership}{
#'   vector of integers in [1, \code{n}] indicating to which cluster each
#'   observation belongs.
#' }
#' \item{sizes}{number of observations in each cluster.}
# -----------------------------------------------------------------------------.
#' @keywords internal
#' @export
QuickShiftClusters <- function(g, n) {

  distance <- NULL # Fix R CMD check: no visible binding for global variable

  # Split QuickShift graph into desired number of subgraphs/clusters
  ecut <- mean(sort(E(g)$distance, decreasing = TRUE)[c(n, n+1) - 1])
  V(g)$id <- 1:length(V(g))
  g <- g - E(g)[distance > ecut]

  # Find root observation for each subgraph
  r <- which(igraph::degree(g, mode = "out") == 0)
  if(length(r) != n) stop("unexpected graph structure")

  # Tag each observation with an identifier of the subgraph it belongs to
  qsc <- list(
    membership = rep(NA, length(V(g))),
    sizes      = rep(0, n),
    nbr        = n
  )
  for(k in 1:n) {
    sg <- subcomponent(g, v = r[k], mode = "in")
    qsc$membership[sg$id] <- k
  }
  # TODO: use tabulate instead of table
  qsc$sizes <- as.vector(table(qsc$membership))

  # Reallocate subgraph/cluster ids by decreasing population sizes
  o <- order(qsc$sizes, decreasing = TRUE, na.last = TRUE)
  qsc$sizes <- qsc$sizes[o]
  qsc$membership <- o[qsc$membership]

  qsc
}

# =============================================================================.
#' Hierarchical clustering based on density gradient ascent
# -----------------------------------------------------------------------------.
#' @seealso
#'   \link{QuickShiftAlgorithm},
#'   \link{QuickShiftClusters}
# -----------------------------------------------------------------------------.
#' @inherit QuickShiftAlgorithm references
#' @inheritParams QuickShiftAlgorithm
#'
#' @param n
#' desired number of clusters.
#'
#' @param k
#' number of nearest neighbors for density estimation (default = 128).
#'
#' @param qsk
#' \strong{EXPERIMENTAL}: number of nearest neighbors for QuickShift
#' (default = 2).
#'
#' @inherit QuickShiftClusters return
# -----------------------------------------------------------------------------.
#' @export
QuickShiftClustering <- function (x, d = NULL, n, k = 128, qsk = 2) {

  if(is.null(d)) d <- knn_density(x, k = k)
  if(qsk <= 2) qs <- QuickShiftAlgorithm(x, d)
  else qs <- QuickShiftAlgorithmAlt(x, d, k = qsk)
  qs <- QuickShiftClusters(qs, n = n)
  qs
}
