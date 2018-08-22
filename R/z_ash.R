# =============================================================================.
#' Univariate density estimation for given observations
# -----------------------------------------------------------------------------.
#' @seealso
#'   \link{ASH2D}
# -----------------------------------------------------------------------------.
#' @param x
#' numeric vector.
#'
#' @param data
#' numeric vector (optional).
#'
#' @param n
#' number of bins.
#'
#' @param k
#' smoothing in number of bins.
#'
#' @param rx
#' range expansion.
#'
#' @param safe
#' logical (defautl = TRUE, yes).
#'
#' @param ...
#' optional argument (\code{kopt}) forwarded to the \link{bin1} function.
#'
#' @return
#' \code{ASH1D} returns a numeric vector with estimated densities.
# -----------------------------------------------------------------------------.
#' @examples
#'
#' \dontrun{
#'
#' n1 <- 30000
#' n2 <- 60000
#' g <- c(rep(1, n1), rep(2, n2))
#' x <- c(rnorm(n1, 0, 1), rnorm(n2, 0, 5))
#'
#' o <- order(x)
#' x <- x[o]
#' g <- g[o]
#'
#' dt <- ASH1D(x, k = 5)
#' d1 <- ASH1D(x, data = x[g == 1], k = 10)
#' d2 <- ASH1D(x, data = x[g == 2], k = 10)
#'
#' clr <- c(
#'   dt        = grey(0.0, 0.6),
#'   d1        = rgb(1.0, 0.5, 0.0, 0.6),
#'   d2        = rgb(0.0, 0.5, 1.0, 0.6),
#'   `d1 + d2` = rgb(1.0, 0.0, 0.0, 0.6)
#' )
#'
#' EmptyPlot(xlim = range(x), ylim = range(dt, d1, d2, d1 + d2))
#' par(lwd = 1.5)
#' lines(x, d1 + d2, col = clr[4])
#' lines(x, dt, col = clr["dt"])
#' lines(x, d1, col = clr["d1"])
#' lines(x, d2, col = clr["d2"])
#' legend("topright", legend = names(clr), fill = clr, bty = "n")
#' }
# -----------------------------------------------------------------------------.
#' @keywords internal
#' @export
ASH1D <- function(
  x, data = NULL, n = 200, k = 5, r = NULL, rx = 1.1, safe = TRUE, ...
) {

  # Initializations
  if(is.null(data)) {
    data <- x
    s <- 1
  } else {
    s <- length(data) / length(x)
  }
  k[k == 0] <- 1

  # Range
  if(is.null(r)) {
    if(safe) r <- range(data)
    else     r <- range(data[FiniteValues(data)])
  }

  # Average Shifted Histogram
  d <- ash::bin1(data, ab = rx * r, nbin = n)
  d <- ash::ash1(d, m = k, ...)

  # Interpolation
  d <- stats::approx(d$x, d$y, xout = x, rule = 2)

  # Density estimation
  d <- s * d$y

  # Cleanup
  d[is.na(d)] <- 0
  d[d < 0] <- 0

  d
}

# =============================================================================.
#' Bivariate density estimation for given observations
# -----------------------------------------------------------------------------.
#' @seealso
#'   \link{ASH1D}
# -----------------------------------------------------------------------------.
#' @param V
#' numeric matrix with 2 columns.
#'
#' @param data
#' numeric matrix with 2 columns (optional).
#'
#' @param n
#' number of bins.
#'
#' @param k
#' smoothing in number of bins.
#'
#' @param rx
#' range expansion.
#'
#' @param safe
#' logical (defautl = TRUE, yes).
#'
#' @param ...
#' optional argument forwarded to the \link{ash2} function.
#'
#' @return
#' \code{ASH2D} returns a numeric vector with estimated densities.
# -----------------------------------------------------------------------------.
#' @examples
#'
#' \dontrun{
#'
#' n1 <- 10000
#' n2 <- 40000
#' g <- c(rep(1, n1), rep(2, n2))
#' V <- cbind(
#'   c(rnorm(n1, 0, 1), rnorm(n2, 0, 5)),
#'   c(rnorm(n1, 0, 1), rnorm(n2, 0, 5))
#' )
#'
#' dt <- ASH2D(V, k = c(5, 5))
#' d1 <- ASH2D(V, data = V[g == 1, ], k = c(10, 10))
#' d2 <- ASH2D(V, data = V[g == 2, ], k = c(10, 10))
#'
#' # Preview of densities using projection on the x axis
#'
#' layout(matrix(1:4, 2, 2, byrow = TRUE))
#'
#' ylim <- range(dt, d1, d2, d1 + d2)
#' r <- BivariateDensity(
#'   V[, 1], d1, ylim = ylim, xlab = "x", ylab = "d", main = "d1"
#' )
#' r <- BivariateDensity(
#'   V[, 1], d2, ylim = ylim, xlab = "x", ylab = "d", main = "d2"
#' )
#' r <- BivariateDensity(
#'   V[, 1], d1 + d2, ylim = ylim, xlab = "x", ylab = "d", main = "d1 + d2"
#' )
#' r <- BivariateDensity(
#'   V[, 1], dt, ylim = ylim, xlab = "x", ylab = "d", main = "dt"
#' )
#'
#' # Scatteplots showing densities as shades of grey
#'
#' o <- order(dt)
#' r <- range(V)
#'
#' layout(matrix(1:4, 2, 2, byrow = TRUE))
#' par(pch = 20)
#'
#' EmptyPlot(xlim = r, ylim = r, main = "d1")
#' points(V[o, ], col = colorize(d1[o], color = "WGB"), cex = 0.5)
#'
#' EmptyPlot(xlim = r, ylim = r, main = "d2")
#' points(V[o, ], col = colorize(d2[o], color = "WGB"), cex = 0.5)
#'
#' EmptyPlot(xlim = r, ylim = r, main = "d1 + d2")
#' points(V[o, ], col = colorize((d1 + d2)[o], color = "WGB"), cex = 0.5)
#'
#' EmptyPlot(xlim = r, ylim = r, main = "dt")
#' points(V[o, ], col = colorize(dt[o], color = "WGB"), cex = 0.5)
#' }
# -----------------------------------------------------------------------------.
#' @keywords internal
#' @export
ASH2D <- function(
  V, data = NULL, n = c(200, 200), k = c(5, 5), rx = c(1.1, 1.1), safe = TRUE,
  interpolation = c("fields", "akima"), ...
) {

  interpolation <- match.arg(interpolation)

  # Initializations
  if(is.null(data)) {
    data <- V
    s <- 1
  } else {
    s <- nrow(data) / nrow(V)
  }

  VectorArgs(c("n", "k", "rx"), size = 2)
  k[k == 0] <- 1

  # Range
  if(safe) r <- t(apply(data, 2, range))
  else     r <- t(apply(data[FiniteValues(data), ], 2, range))

  # Average Shifted Histogram
  d <- ash::bin2(data, nbin = n)
  d <- ash::ash2(d, m = k, ...)

  # Interpolation
  if(interpolation == "fields") { # fastest
    d <- fields::interp.surface(list(x = d$x, y = d$y, z = d$z), V)
  }
  if(interpolation == "akima") {
    d <- akima::bilinear(x = d$x, y = d$y, z = d$z, x0 = V[, 1], y0 = V[, 2])$z
  }

  # Density estimation
  d <- s * d

  # Cleanup
  d[is.na(d)] <- 0
  d[d < 0] <- 0

  d
}
