# =============================================================================.
#' Plot a QuickShift graph
# -----------------------------------------------------------------------------.
#' @param x
#' numeric matrix representing 2D coordinates of the data points.
#'
#' @param g
#' a QuickShift graph resulting from the \link{QuickShiftAlgorithm} function.
#'
#' @param new
#' logical value whether to start a new plot (default = TRUE, yes).
#'
#' @param length
#' numeric value controlling arrow lengths (default = 0.05).
#'
#' @param col
#' arrow colors (\code{grey(0, 0.2)} by default).
#'
#' @param ...
#' optional arguments passed to the \link{arrows} function.
#'
#' @return NULL
# -----------------------------------------------------------------------------.
#' @export
PlotQuickShift <- function(
  x, g, new = TRUE, length = 0.05, col = grey(0, 0.2), ...
) {

  if(new) plot(x[, 1], x[, 2], type='n')

  el <- igraph::as_edgelist(g)
  suppressWarnings(
    arrows(
      x[el[, 1], 1], x[el[, 1], 2], x[el[, 2], 1], x[el[, 2], 2],
      length = length, col = col, ...
    )
  )
}
