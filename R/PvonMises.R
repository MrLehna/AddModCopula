#' Von Mises distribution function
#'
#' @description This functions returns the probability of the
#' von Mises distribution. Based on the circular package
#'
#' @param q Input parameter
#' @param mu Mu parameter
#' @param kappa Kappa parameter
#' @param tol Tollerance level for the probability calculation
#' @return Returns probability value

PvonmisesRad <- function(q, mu, kappa, tol=1e-020) {
  q <- q %% (2 * pi)
  n <- length(q)
  mu <- mu %% (2 * pi)
  pvm.mu0 <- function(q, kappa, tol) {
    flag <- TRUE
    p <- 1
    sum <- 0
    while (flag) {
      term <- (besselI(x=kappa, nu=p, expon.scaled = FALSE) * sin(p * q))/p
      sum <- sum + term
      p <- p + 1
      if (all(abs(term) < tol))
        flag <- FALSE
    }
    return(q/(2 * pi) + sum/(pi * besselI(x=kappa, nu=0, expon.scaled = FALSE)))
  }

  upper <- q - mu
  upper[which(q <= mu)] <- (q[which(q <= mu)] - mu[which(q <= mu)]) %% (2 * pi)
  upper[which(upper == 0)] <- 2 * pi

  lower <- mu %% (2 * pi)
  lower[which(q <= mu)] <- ( - mu[which(q <= mu)]) %% (2 * pi)

  result <- pvm.mu0(upper, kappa, tol) + pvm.mu0(lower, kappa, tol)
  result[which(q <= mu)] <- pvm.mu0(upper[which(q <= mu)], kappa[which(q <= mu)], tol) -
    pvm.mu0(lower[which(q <= mu)], kappa[which(q <= mu)], tol)
  result[which(mu == 0)] <- pvm.mu0(q[which(mu == 0)], kappa[which(mu == 0)], tol)

  return(result)
}
