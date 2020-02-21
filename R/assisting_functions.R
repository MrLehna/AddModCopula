#' Assisting function for the creg_calc function
#'
#' @description This function calculates the density and probabilty of a given
#' distribution
#'
#' @param Y Vector, Input for the distribution
#' @param param Parameter of the distribution. These can range from 1-5
#' parameters.
#' @param k Number of parameters of the distribution
#' @param dist Distribution. This can either be a list, vonMises or from the
#' dist() package
#'
#' @details See the documentation of global_creg for more information
#'
#'
#' @return Returns a matrix: first collumn is the probability, second the
#' density
#'
#' @import distr

global_dist_calc <- function(Y,param,k,dist){

  # Functions for dist() package distributions, which only accept integers
  calcdist1 <- function(P,dist){
    X <- dist(P[2])
    u <- d(X) (P[1])
    cu <- p(X)(P[1])
    return(c(u,cu))
  }
  calcdist2 <- function(P,dist){
    X <- dist(P[2],P[3])
    u <- d(X) (P[1])
    cu <- p(X)(P[1])
    return(c(u,cu))
  }

  calcdist3 <- function(P,dist){
    X <- dist(P[2],P[3],P[4])
    u <- d(X) (P[1])
    cu <- p(X)(P[1])
    return(c(u,cu))
  }

  calcdist4 <- function(P,dist){
    X <- dist(P[2],P[3],P[4],P[5])
    u <- d(X) (P[1])
    cu <- p(X)(P[1])
    return(c(u,cu))
  }

  calcdist5 <- function(P,dist){
    X <- dist(P[2],P[3],P[4],P[5],P[6])
    u <- d(X) (P[1])
    cu <- p(X)(P[1])
    return(c(u,cu))
  }
  calclist1 <- function(P,distlist){
    u <- distlist[[1]](x=P[1],P[2])
    cu <- distlist[[2]](q=P[1],P[2])
    return(c(u,cu))
  }
  calclist2 <- function(P,distlist){
    u <- distlist[[1]](x=P[1],P[2],P[3])
    cu <- distlist[[2]](q=P[1],P[2],P[3])
    return(c(u,cu))
  }

  calclist3 <- function(P,distlist){
    u <- distlist[[1]](x=P[1],P[2],P[3],P[4])
    cu <- distlist[[2]](q=P[1],P[2],P[3],P[4])
    return(c(u,cu))
  }

  calclist4 <- function(P,distlist){
    u <- distlist[[1]](x=P[1],P[2],P[3],P[4],P[5])
    cu <- distlist[[2]](q=P[1],P[2],P[3],P[4],P[5])
    return(c(u,cu))
  }

  calclist5 <- function(P,distlist){
    u <- distlist[[1]](x=P[1],P[2],P[3],P[4],P[5],P[6])
    cu <- distlist[[2]](q=P[1],P[2],P[3],P[4],P[5],P[6])
    return(c(u,cu))
  }

  calcvonmises <- function(P){
    cu<- PvonmisesRad (q=P[1], mu= P[2],kappa = P[3])
    u <- DvonmisesRad (x=P[1], mu= P[2],kappa = P[3])
    return(c(u,cu))
      }
# Now function for all distr:
# The return is allways first the density, then the probability
    if (is.character(dist)==TRUE){
      # Raum fuer Notizen
      if (dist=="vonMises"&k==2){
        out <- cbind(DvonmisesRad(x=Y,mu=param[,1],kappa=param[,2]),
                     PvonmisesRad(q=Y,mu=param[,1],kappa=param[,2]))
      }else{stop("This distr. is not known. Do you mean vonMises?")}
      }else{if (is.list(dist)==TRUE){
       if(k==1){out <- apply(cbind(Y,param),1,FUN=calclist1,dist=dist)}else{
         if(k==2){out <- apply(cbind(Y,param),1,FUN=calclist2,dist=dist)}else{
           if(k==3){out <- apply(cbind(Y,param),1,FUN=calclist3,dist=dist)}else{
             if(k==4){out <- apply(cbind(Y,param),1,FUN=calclist4,dist=dist)}else{
               if(k==5){out <- apply(cbind(Y,param),1,FUN=calclist5,dist=dist)}else{
                 stop("Only distributions up to 5 parameters are supported")}
             }}}}}else{
               if(k==1){out <- apply(cbind(Y,param),1,
                                     FUN=calcdist1,dist=dist)}else{
                 if(k==2){out <- apply(cbind(Y,param),1,
                                       FUN=calcdist2,dist=dist)}else{
                   if(k==3){out <- apply(cbind(Y,param),1,
                                         FUN=calcdist3,dist=dist)}else{
                     if(k==4){out <- apply(cbind(Y,param),1,
                                           FUN=calcdist4,dist=dist)}else{
                       if(k==5){out <- apply(cbind(Y,param),1,
                                             FUN=calcdist5,dist=dist)}else{
                   stop("Only distributions up to 5 parameters are supported")}
                                           }}}}}
        out <- t(out)
      }

    colnames(out) <- c("Density","Prob.")
    return(out)
  }
