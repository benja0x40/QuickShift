# COMMON #######################################################################

# =============================================================================.
#' ** RESERVED FOR INTERNAL USE **
# -----------------------------------------------------------------------------.
#' @description
#' Convert matrix indices into vector indices such that:
#'
#' x <- m2v(i, j) => M[i, j] = M[x] for each pair of indices
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
#' Convert vector indices into matrix indices such that:
#'
#' ij <- v2m(x) => M[x] = M[ij[, 1], ij[, 2]] for each pair of indices
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
