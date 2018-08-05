# =============================================================================.
#' Optimal match between two different cluster/class assignments
# -----------------------------------------------------------------------------.
#' @param x
#' integer vector representing a set of cluster/class memberships.
#'
#' @param r
#' integer vector representing a set of alternative cluster/class memberships
#' (e.g. reference or ground truth assignments).
#'
#' @param n
#' total number of classes, inferred from argument \code{x} when not provided.
#'
#' @return
#' \code{MatchClusters} returns a matrix with the following columns:
#' \item{x2r}{x to r cluster/class translation}
#' \item{r2x}{r to x cluster/class translation}
# -----------------------------------------------------------------------------.
#' @export
MatchClusters <- function(x, r, n = NULL) {
  # Number of classes
  if(is.null(n)) n <- length(tabulate(x))
  # Compute all intersections
  tbl <- matrix(0, n, n)
  for(i in 1:n) {
    for(j in 1:n) {
      tbl[i, j] <- sum(x == i & r == j)
    }
  }
  # Make optimal associations
  idx <- matrix(0, n, 2, dimnames = list(NULL, c("x2r", "r2x")))
  o <- QuickShift::v2m(order(tbl, decreasing = TRUE), nrow = n)
  for(k in 1:(n * n)) {
    if(idx[o[k, 1], 1] == 0 & idx[o[k, 2], 2] == 0) {
      idx[o[k, 1], 1] <- o[k, 2]
      idx[o[k, 2], 2] <- o[k, 1]
    }
  }
  idx
}
