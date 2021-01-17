# =============================================================================.
#' Hierarchical clustering based on the estimated density gradient
# -----------------------------------------------------------------------------.
#' @seealso
#'   \link{QuickShiftAlgorithm},
#'   \link{QuickShiftClusters},
#'   \link{knn_density}
# -----------------------------------------------------------------------------.
#' @inheritParams QuickShiftAlgorithm
#' @inherit QuickShiftAlgorithm references
#' @inherit QuickShiftClusters return
#'
#' @param n
#' desired number of clusters.
#'
#' @param k
#' number of nearest neighbors for density estimation (used only when argument
#' \code{d} is not provided).
#'
#' @param ...
#' optional arguments passed to the \link{QuickShiftAlgorithm} or
#' \link{QuickShiftClusters} functions.
# -----------------------------------------------------------------------------.
#' @import utils
#' @import grDevices
#' @import graphics
#' @import igraph
#' @importFrom matrixStats colSums2 colMeans2 rowSums2 rowMeans2
#' @importFrom FNN get.knn get.knnx knnx.dist
# -----------------------------------------------------------------------------.
#' @export
QuickShift <- function (x, n, d = NULL, k = NULL, ...) {

  if(is.null(d) & is.null(k)) k <- min(round(sqrt(nrow(x))/2), 256)

  # Arguments forwarded to lower level functions
  a <- list(...)
  qsa <- names(a) %in% methods::formalArgs(QuickShiftAlgorithm)
  qsc <- names(a) %in% methods::formalArgs(QuickShiftClusters)

  # Density estimation if not provided
  if(is.null(d)) d <- knn_density(x, k = k)

  # Clustering
  r <- do.call(QuickShiftAlgorithm, c(list(x = x, d = d), a[qsa]))
  r <- do.call(QuickShiftClusters, c(list(g = r, n = n), a[qsc]))
  r
}
