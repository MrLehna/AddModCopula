# This file is used to define background informations, such as data description
# as well as define global variables


#' The following dataset is available in the AddModCopula
#'
#' @name helgoland_wind
#' @docType data
#' @author Malte Lehna
#' @description The dataset contains the hourly observations of wind speed and
#' wind direction of the island Helgoland. The collumns are defined as followed:
#'
#' ws: wind speed in m/s
#' wd: wind direction, ranging from 0 to 2*pi
#' dwd: describes the derivation of wd (see example for the code)
#'
#'
#' @references \url{https://www.dwd.de/}
#' @keywords data
#' @examples
#'calc.dwd <- function(wd) {
#'dwd <- rep(0, length(wd))
#'minvec <- c(0, 0, 0)
#'
#'for (i in 2 : length(wd)){
#'  # Version for 2 pi scale
#'  minvec[1] <- wd[i] - wd[i-1]
#'  minvec[2] <- (wd[i] + 2 * pi) - wd[i-1]
#'  minvec[3] <- (wd[i] - 2 * pi) - wd[i-1]
#'  dwd[i] <- minvec[min(abs(minvec)) == abs(minvec)]
#'  }
#'return(dwd)
#'}
#'
NULL

utils::globalVariables(c("Y" ,"density" ,"scatter3d"))
