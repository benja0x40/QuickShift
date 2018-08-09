# =============================================================================.
#' Optimal match between two different cluster/class assignments
# -----------------------------------------------------------------------------.
#' @param x
#' integer vector representing a set of cluster/class memberships.
#'
#' @param r
#' integer vector representing an alternative set of cluster/class memberships
#' (e.g. reference or ground truth assignments).
#'
#' @param n
#' total number of clusters/classes
#' (inferred from argument \code{x} when not provided).
#'
#' @return
#' \code{MatchClusters} returns an integer matrix with the following columns:
#' \item{x2r}{translation from x to r cluster/class identifiers}
#' \item{r2x}{translation from r to x cluster/class identifiers}
#' For instance, noting \code{m} such translation matrix, then
#' \item{m[3, "x2r"]}{
#'   is the best matching cluster identifier \code{r} for the cluster 3
#'   in classification \code{x}
#' }
#' \item{m[5, "r2x"]}{
#'   is the best matching cluster identifier \code{x} for the cluster 5
#'   in classification \code{r}
#' }
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
