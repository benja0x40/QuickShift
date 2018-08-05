# =============================================================================.
#' Apply dithering to read counts
# -----------------------------------------------------------------------------.
#' @description
#' The \code{DitherCounts} function applies a triangular dithering filter
#' which eliminates the presence of identical values in a read count matrix.
#'
#' @param x
#' matrix of read counts (rows = observations, columns = samples or conditions).
#'
#' @return
#' \code{DitherCounts} returns a matrix of dithered counts.
# -----------------------------------------------------------------------------.
#' @keywords internal
#' @export
DitherCounts <- function(x) {

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
