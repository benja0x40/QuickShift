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
#' @param progress
#' show progress (logical, default = FALSE).
#'
#' @return
#' \code{QuickShiftAlgorithm} returns a graph object (see \link{igraph} package).
# -----------------------------------------------------------------------------.
#' @import methods
#' @import igraph
#' @importFrom utils txtProgressBar setTxtProgressBar
#' @importFrom matrixStats colSums2 colMeans2 rowSums2 rowMeans2
#' @importFrom FNN get.knn get.knnx knnx.dist
# -----------------------------------------------------------------------------.
#' @keywords internal
#' @export
QuickShiftAlgorithm <- function (x, d, progress = FALSE) {

  if(progress) pb <- txtProgressBar(min = 1, max = nrow(x), char = "|", style= 3)

  g <- graph.empty(n = nrow(x))
  i.a <- which(FiniteValues(x) & ! is.na(d))

  while(length(i.a) > 1) {

    knn <- get.knnx(
      data = x[i.a, ], query = x[i.a, ], k = 2, algorithm = "kd_tree"
    )

    i.b <- i.a[knn$nn.index[, 2]]
    chk <- d[i.b] >= d[i.a]

    g <- g + igraph::edges(
      rbind(i.a, i.b)[, chk], distance = knn$nn.dist[chk, 2]
    )
    i.a <- i.a[! chk]

    if(progress) setTxtProgressBar(pb, nrow(x) - length(i.a) + 1)
  }
  if(progress) close(pb)

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
#' @param ...
#'
#' @inherit QuickShiftClusters return
# -----------------------------------------------------------------------------.
#' @export
QuickShiftClustering <- function (x, d = NULL, n, k = 128, ...) {

  if(is.null(d)) d <- knn_density(x, k = k)
  qs <- QuickShiftAlgorithm(x, d, ...)
  qs <- QuickShiftClusters(qs, n = n)
  qs
}
