# =============================================================================.
#' Clustering from a QuickShift graph
# -----------------------------------------------------------------------------.
#' @seealso
#'   \link{QuickShift},
#'   \link{MatchClusters}
# -----------------------------------------------------------------------------.
#' @param g
#' a QuickShift graph resulting from the \link{QuickShiftAlgorithm} function.
#'
#' @param n
#' desired number of clusters.
#'
#' @param graph
#' logical value indicating whether the QuickShift graph should be part of
#' the returned results (default = FALSE, no).
#'
#' @param id_by_pop
#' logical value controlling if cluster identifiers should follow the order
#' of population sizes (default = TRUE, yes).
#'
#' @param decreasing
#' logical value indicating if population sizes should be in decreasing
#' (default = TRUE, yes) or increasing order, when the argument
#' \code{id_by_pop} is TRUE.
#'
#' @return
#' This function returns a list with the following elements:
#' \item{membership}{
#'   vector of integers in [1, \code{nclust}] indicating to which cluster each
#'   observation belongs.
#' }
#' \item{csizes}{number of observations in each cluster.}
#' \item{nclust}{number of clusters.}
#' And optionally:
#' \item{graph}{the QuickShift graph (see \link{QuickShiftAlgorithm}).}
# -----------------------------------------------------------------------------.
#' @export
QuickShiftClusters <- function(
  g, n, graph = FALSE, id_by_pop = TRUE, decreasing = TRUE
) {

  distance <- NULL # Fix R CMD check: no visible binding for global variable

  # Find the root observations
  roots <- which(igraph::degree(g, mode = "out") == 0)

  # Split QuickShift graph into desired number of subgraphs/clusters
  k <- n - length(roots)
  if(k > 0) {
    ecut <- mean(sort(igraph::E(g)$distance, decreasing = TRUE)[k + 0:1])
    igraph::V(g)$id <- 1:length(igraph::V(g))
    g <- g - igraph::E(g)[distance >= ecut]
  }

  # Update roots
  roots <- which(igraph::degree(g, mode = "out") == 0)

  if(length(roots) != n) {
    n <- length(roots)
    warning("unexpected graph structure")
  }

  # Result template
  r <- list(
    membership = rep(NA, length(igraph::V(g))),
    csizes     = rep(0, n),
    nclust     = n
  )

  # Tag each observation with an identifier of the subgraph it belongs to
  for(k in 1:n) {
    sg <- igraph::subcomponent(g, v = roots[k], mode = "in")
    r$membership[sg$id] <- k
  }

  # Population sizes
  r$csizes <- table(r$membership)

  # Reallocate subgraph/cluster ids by sorted population sizes
  if(id_by_pop) {
    o <- order(r$csizes, decreasing = decreasing, na.last = TRUE)
    r$membership <- o[r$membership]
    r$csizes <- r$csizes[o]
    names(r$csizes) <- 1:length(r$csizes)
  }

  if(graph) r$graph <- g

  r
}
