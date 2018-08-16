# COMMON #######################################################################

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
