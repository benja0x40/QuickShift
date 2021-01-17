# FUNCTIONS | KNN STATS ########################################################

# =============================================================================.
#' Extract multivariate local values based on a knn index matrix
# -----------------------------------------------------------------------------.
#' @param v
#' numeric vector.
#'
#' @param i
#' precomputed matrix of nearest neighbor indexes.
#'
#' @return
#' \code{knn_values} returns a matrix.
# -----------------------------------------------------------------------------.
#' @keywords internal
#' @export
knn_values <- function(v, i) {

  matrix(v[as.vector(i)], nrow(i), ncol(i))
}

# =============================================================================.
#' Apply a smoothing function to multivariate local values
# -----------------------------------------------------------------------------.
#' @inheritParams knn_values
#'
#' @param f
#' smoothing function (default = mean).
#'
#' @return
#' \code{knn_smoothing} returns a numeric vector.
# -----------------------------------------------------------------------------.
#' @keywords internal
#' @export
knn_smoothing <- function(v, i, f = mean) {

  v <- knn_values(v, i)
  v <- apply(v, 1, f)

  v
}

# =============================================================================.
#' Non-parametric multivariate density estimator
# -----------------------------------------------------------------------------.
#' @seealso
#'   \link{knn_mean}
# -----------------------------------------------------------------------------.
#' @description
#' Standard k-nearest neighbor (knn) density estimator \deqn{P(Xi) ~ k / (N Vi)}
#' where \eqn{N} is the number of observations and \eqn{Vi} the volume of a
#' sphere of radius equal to the distance between \eqn{Xi} and its k-nearest
#' neighbor.
#'
#' @param x
#' numeric matrix representing multivariate data where rows = observations
#' and columns = measurement conditions (query data).
#'
#' @param k
#' number of nearest neighbors which has equivalent effects as the
#' usual bandwidth or smoothing parameters (larger k => smoother results).
#'
#' @param xref
#' numeric matrix representing multivariate data where rows = observations
#' and columns = measurement conditions (optional reference data).
#'
#' @param smoothing
#' logical value activating an additional local average smoothing of
#' the estimated density (default = TRUE, active).
#'
#' @param nn_alg
#' nearest neighbor searching algorithm (default = "kd_tree").
#' See \link{get.knn} for further explanations.
#'
#' @return
#' \code{knn_density} returns a numeric vector.
# -----------------------------------------------------------------------------.
#' @examples
#'
#' \dontrun{
#'
#' n1 <- 10000
#' n2 <- 20000
#' g <- c(rep(1, n1), rep(2, n2))
#' x <- c(rnorm(n1, 0, 1), rnorm(n2, 0, 5))
#'
#' o <- order(x)
#' x <- x[o]
#' g <- g[o]
#'
#' dt <- knn_density(x, k = 50)
#' d1 <- knn_density(x, xref = x[g == 1], k = 50)
#' d2 <- knn_density(x, xref = x[g == 2], k = 50)
#'
#' clr <- c(
#'   dt        = grey(0.0, 0.6),
#'   d1        = rgb(1.0, 0.5, 0.0, 0.6),
#'   d2        = rgb(0.0, 0.5, 1.0, 0.6),
#'   `d1 + d2` = rgb(1.0, 0.0, 0.0, 0.6)
#' )
#'
#' plot(0, type = 'n', xlim = range(x), ylim = range(dt, d1, d2, d1 + d2))
#' par(lwd = 1.5)
#' lines(x, d1 + d2, col = clr[4])
#' lines(x, dt, col = clr["dt"])
#' lines(x, d1, col = clr["d1"])
#' lines(x, d2, col = clr["d2"])
#' legend("topright", legend = names(clr), fill = clr, bty = "n")
#'
#' }
# -----------------------------------------------------------------------------.
#' @export
knn_density <- function(
  x, k, xref = NULL, smoothing = TRUE, nn_alg = "kd_tree"
) {

  x <- as.matrix(x)

  N  <- nrow(x) # number of observations
  D  <- ncol(x) # number of dimensions of each observation

  if(is.null(xref)) {
    r <- get.knn(data = x, k = k, algorithm = nn_alg)
  } else {
    r <- get.knnx(data = xref, query = x, k = k, algorithm = nn_alg)
    r$nn.index <- get.knn(data = x, k = k, algorithm = nn_alg)$nn.index
  }

  # Compute density in any dimensions D
  V1 <- pi ^ (D / 2) / factorial(D / 2)  # Volume of the unit sphere
  p <- k / (N * V1 * r$nn.dist[, k] ^ D) # Standard knn density estimator

  if(smoothing) p <- knn_smoothing(p, r$nn.index)

  p
}

# =============================================================================.
#' Multivariate local means and variances
# -----------------------------------------------------------------------------.
#' @seealso
#'   \link{knn_density},
#'   \link{knn_mean}
# -----------------------------------------------------------------------------.
#' @description
#' k-nearest neighbor (knn) centroid and associated local standard deviation.
#'
#' @inheritParams knn_density
#'
#' @param smoothing
#' logical value activating an additional local average smoothing of
#' the estimated variances (default = FALSE, inactive).
#'
#' @return
#' \code{knn_musigma2} returns a list with the following elements:
#' \item{mu}{knn centroids}
#' \item{sigma2}{knn variances (squared standard deviations)}
# -----------------------------------------------------------------------------.
#' @keywords internal
#' @export
knn_musigma2 <- function(
  x, k, xref = NULL, smoothing = FALSE, nn_alg = "kd_tree"
) {

  x <- as.matrix(x)

  N  <- nrow(x) # number of observations
  D  <- ncol(x) # number of dimensions of each observation

  m <- matrix(0, N, D) # mean vectors (centroids)

  if(is.null(xref)) {

    # compute centroids
    r <- get.knn(data = x, k = k, algorithm = nn_alg)
    for(j in 1:D) m[, j] <- rowMeans(knn_values(x[, j], r$nn.index))

    # compute average distances to centroids
    v <- rowMeans(knnx.dist(data = x, query = m, k = k)^2)

  } else {

    # compute centroids
    r <- get.knnx(data = xref, query = x, k = k, algorithm = nn_alg)
    for(j in 1:D) m[, j] <- rowMeans(knn_values(xref[, j], r$nn.index))

    # compute average distances to centroids
    v <- rowMeans(knnx.dist(data = xref, query = m, k = k)^2)

    r$nn.index <- get.knn(data = x, k = k, algorithm = nn_alg)$nn.index
  }

  if(smoothing) v <- knn_smoothing(v, r$nn.index)

  list(mu = m, sigma2 = v)
}

# =============================================================================.
#' Multivariate local means
# -----------------------------------------------------------------------------.
#' @seealso
#'   \link{knn_density}
# -----------------------------------------------------------------------------.
#' @description
#' compute k-nearest neighbor (knn) centroids (i.e. mean vectors).
#'
#' @inheritParams knn_density
#'
#' @return
#' \code{knn_mean} returns a matrix of knn centroids.
# -----------------------------------------------------------------------------.
#' @export
knn_mean <- function(x, k, xref = NULL, nn_alg = "kd_tree") {

  x <- as.matrix(x)

  N  <- nrow(x) # number of observations
  D  <- ncol(x) # number of dimensions of each observation

  m <- matrix(0, N, D) # mean vectors (centroids)

  if(is.null(xref)) {

    r <- get.knn(data = x, k = k, algorithm = nn_alg)
    for(j in 1:D) m[, j] <- rowMeans(knn_values(x[, j], r$nn.index))

  } else {

    r <- get.knnx(data = xref, query = x, k = k, algorithm = nn_alg)
    for(j in 1:D) m[, j] <- rowMeans(knn_values(xref[, j], r$nn.index))

  }

  m
}
