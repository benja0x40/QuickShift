# =============================================================================.
#' PlotQuickShift
# -----------------------------------------------------------------------------.
#' @param x matrix
#' @param g graph
#' @param ...
#'
#' @return NULL
# -----------------------------------------------------------------------------.
#' @keywords internal
#' @export
PlotQuickShift <- function(
  x, g, new = TRUE, length = 0.05, col = rgb(0, 0, 0, 0.2), ...
) {

  if(new) plot(x[, 1], x[, 2], type='n')

  el <- as_edgelist(g)
  suppressWarnings(
    arrows(
      x[el[, 1], 1], x[el[, 1], 2], x[el[, 2], 1], x[el[, 2], 2],
      length = length, col = col, ...
    )
  )
}
