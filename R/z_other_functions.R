# EXPOSED ######################################################################

# =============================================================================.
#' Apply dithering to integer values
# -----------------------------------------------------------------------------.
#' @description
#' The \code{Dithering} function applies a triangular dithering filter
#' which should eliminate the presence of identical non-zero values
#' in the input vector or matrix, without distorting the original
#' distribution of values.
#'
#' @param x
#' integer vector or matrix.
#'
#' @return
#' \code{Dithering} returns a numeric vector or matrix.
# -----------------------------------------------------------------------------.
#' @export
Dithering <- function(x) {

  zero <- x == 0
  xmin <- min(x[! zero], na.rm = TRUE)
  xmax <- max(x[! zero], na.rm = TRUE)

  # dithering
  x <- x + triangle::rtriangle(length(x), a = -1, b = 1)
  # lower limit
  k <- x < xmin & ! zero
  x[k] <- stats::runif(sum(k), xmin - 0.5, xmin)
  # upper limit
  k <- x > xmax
  x[k] <- stats::runif(sum(k), xmax, xmax + 0.5)
  # no count
  x[zero] <- 0

  x
}

# HIDDEN #######################################################################

# =============================================================================.
#' Localize safe numeric observations (i.e. not NA, NaN, Inf)
# -----------------------------------------------------------------------------.
#' @seealso
#'   \link{is.finite}
# -----------------------------------------------------------------------------.
#' @param x
#' numeric vector or matrix.
#'
#' @return
#' \code{FiniteValues} returns a logical vector.
# -----------------------------------------------------------------------------.
#' @keywords internal
#' @export
FiniteValues <- function(x) {

  x <- is.finite(x)

  if(! is.null(dim(x))) {
    # x <- Rfast::rowsums(x, parallel = TRUE) == ncol(x)
    x <- matrixStats::rowSums2(x) == ncol(x)
  }

  x
}

# =============================================================================.
#' Rescale x linearly into [0, 1]
# -----------------------------------------------------------------------------.
#' @seealso
#'   \link{RankScore},
#'   \link{SX2Y}
# -----------------------------------------------------------------------------.
#' @description
#' rescale values linearly to the unit interval.
#'
#' @param x
#' numeric vector or matrix.
#'
#' @return
#' \code{S01} returns x linearly rescaled such that range(x) = [0, 1].
# -----------------------------------------------------------------------------.
#' @keywords internal
#' @export
S01 <- function(x) {
  (x - min(x)) / diff(range(x))
}

# =============================================================================.
#' SX2Y
# -----------------------------------------------------------------------------.
#' @seealso
#'   \link{RankScore},
#'   \link{S01}
# -----------------------------------------------------------------------------.
#' @description
#' rescale x values linearly to match the range of y.
#'
#' @param x
#' numeric vector.
#'
#' @param y
#' numeric vector.
#'
#' @return
#' \code{SX2Y} returns x rescaled such that range(x) = range(y).
# -----------------------------------------------------------------------------.
#' @keywords internal
#' @export
SX2Y <- function(x, y) {
  S01(x) * diff(range(y)) + min(y)
}

# =============================================================================.
#' Rescale x non-linearly into ]0, 1[
# -----------------------------------------------------------------------------.
#' @seealso
#'   \link{S01},
#'   \link{SX2Y}
# -----------------------------------------------------------------------------.
#' @description
#' rescale values non-linearly to the unit interval using rank scores \eqn{q}
#' given by \eqn{q = (rank(x) - 0.5) / N} where \eqn{N} = length(x).
#'
#' @param x
#' numeric vector.
#'
#' @return
#' \code{RankScore} returns a numeric vector of rank scores.
# -----------------------------------------------------------------------------.
#' @keywords internal
#' @export
RankScore <- function(x) {
  d <- dim(x)
  n <- colnames(x)
  x <- (rank(x) - 0.5) / length(x)
  if(! is.null(d)) x <- array(x, dim = d)
  if(! is.null(n)) colnames(x) <- n
  x
}

# RESERVED #####################################################################

# =============================================================================.
#' ** RESERVED FOR INTERNAL USE **
# -----------------------------------------------------------------------------.
#' @description
#' Standardize the length of vector arguments.
# -----------------------------------------------------------------------------.
#' @keywords internal
#' @export
VectorArgs <- function(lst, from = NULL, size = NULL) {

  if(is.null(from)) from <- parent.frame()
  if(is.null(size)) {
    size <- 0
    for(x in lst) size <- max(size, length(from[[x]]))
  }

  for(x in lst) from[[x]] <- rep(from[[x]], length.out = size)

  if(! is.environment(from)) from
}

# =============================================================================.
#' ** RESERVED FOR INTERNAL USE **
# -----------------------------------------------------------------------------.
#' @description
#' Convert matrix indexes into vector indexes such that:
#'
#' x <- m2v(i, j) => M[i, j] = M[x] for each pair of indexes
# -----------------------------------------------------------------------------.
#' @keywords internal
#' @export
m2v <- function(i, j, nrow) {
  (j - 1) * nrow + i
}

# =============================================================================.
#' ** RESERVED FOR INTERNAL USE **
# -----------------------------------------------------------------------------.
#' @description
#' Convert vector indexes into matrix indexes such that:
#'
#' ij <- v2m(x) => M[x] = M[ij[, 1], ij[, 2]] for each pair of indexes
# -----------------------------------------------------------------------------.
#' @keywords internal
#' @export
v2m <- function(x, nrow) {
  j <- (x - 1) %/% nrow + 1
  i <- (x - 1) %% nrow + 1
  x <- cbind(i, j)
  attributes(x) <- attributes(x)[1] # remove auto-generated dimnames
  x
}
