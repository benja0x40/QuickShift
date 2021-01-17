# COMMON #######################################################################

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
