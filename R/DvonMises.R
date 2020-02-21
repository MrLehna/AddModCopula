#'  Von Mises distribution function
#'
#' @description This functions returns the density  of the
#' von Mises distribution from the Circular package
#'
#' @param x Input parameter
#' @param mu Mu parameter
#' @param kappa Kappa parameter
#'
#' @return Returns density
#'

DvonmisesRad <- function(x,mu,kappa){
  v <- 1/(2 * pi * besselI(kappa, 0)) * exp(kappa * cos(x - mu))
  v[which(is.nan(v))] <- 0
  return(v)
}

