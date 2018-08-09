# =============================================================================.
#' Apply dithering to integer values
# -----------------------------------------------------------------------------.
#' @description
#' The \code{Dithering} function applies a triangular dithering filter
#' which should eliminate the presence of identical non-zero values
#' in the input vector or matrix, whithout distorting the original
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
