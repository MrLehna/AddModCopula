#' Function to calculate the likelihood of a copula with two distributions
#'
#' @description This function calculates the likehood of a copula from two
#' distributions.
#'
#' @param data Input for the two distributions
#' @param param A matrix/data.frame which defines the parameters for the
#' distributions and the copula.
#' @param dist1 First distribution, see Details
#' @param dist2 Second distribution, see Details
#' @param copula Defining the copula for the calculation. Can either be "Frank",
#' "Gumbel", "Gaussian" or "Clayton".
#' @param param_dim To correctly extract the parameters from the parameter
#' matrix, the number of parameters per distribution has to be defined. This
#' is done by providing a vector with two parameters. E.g. c(2,1)
#'
#' @details This function is primarily used to return the sum of log-likelihood
#' for the optimization of the beta coefficients of the global_creg function.
#' For the exact formulas, please consult the documentation fo global_creg
#'
#' @seealso \code{\link{global_creg}},\code{\link{creg_opt}},
#'
#'
#' @return Depending on the choice of probab, either a likelihood or the result
#' of the copula is returned.

creg_calc<- function(data,param,dist1=Norm(),dist2=Norm(),copula = "Frank",
                     param_dim){


  #############################
  # Distr. Calculation:
  #############################


  # First distr.
  distribution1 <- global_dist_calc(Y=data[,1],
                                    param=param[,c(1:param_dim[1])],
                                    k=param_dim[1],
                                    dist = dist1)
  # Second distr.
  distribution2 <- global_dist_calc(Y=data[,2],
                                    param=param[,c((param_dim[1]+1):
                                                  (param_dim[1]+param_dim[2]))],
                                    k=param_dim[2],
                                    dist = dist2)
  # print(distribution1)
  # print(distribution2)

  u <- distribution1[,1]
  cu <-distribution1[,2]
  v <- distribution2[,1]
  cv <- distribution2[,2]

  u[which(is.nan(u))] <- 0
  v[which(is.nan(v))] <- 0

  #############################
  # Copula Calc:
  #############################



  theta <- param[,(param_dim[1]+param_dim[2]+1):sum(param_dim)]
  #Copular

  if (is.character(copula)==TRUE){
  if (copula == "Gumbel"){
    if (is.vector(theta)==FALSE){if(ncol(theta)>1){
      stop("For the Gumbel Copula only one Theta parameter can be supported per
           observation.")
    }}else{
    if (any(theta <1)){
      stop("For the Gumbel Copula, the Theta parameter can not be smaller then one,
           which is currently not the case. Please transform the parameters
           accordingly.")
    }else{
  t_1 <- (-log(cu))^theta
  t_2<- (-log(cv))^theta
  C <- exp(-1*(t_1+t_2)^(1/theta))
  c <- C*(1/(cu*cv))*(t_1+t_2)^(-2+2/theta)*(log(cu)*log(cv))^(theta-1)*(
    1+(theta-1)*(t_1+t_2)^(-1/theta))

  }}}
  # transformation and function for the Clayton-copula
  if(copula == "Clayton"){
    if (is.vector(theta)==FALSE){if(ncol(theta)>1){
      stop("For the Clayton Copula only one Theta parameter can be supported per
           observation.")
    }}else{
    if (any(theta <=0)){
      stop("For the Clayton Copula, the Theta parameter has to be larger then
      zero,which is currently not the case. Please transform the parameters
           accordingly.")
    }else{
    c <- (1+theta)*(cu*cv)^(-1-theta)*(cu^-theta+cv^-theta-1)^(-1/theta-2)

  }}}

  # transformation and function for the Frank-copula
  if(copula == "Frank"){
    if (is.vector(theta)==FALSE){if(ncol(theta)>1){
      stop("For the Frank Copula only one Theta parameter can be supported per
           observation.")
    }}else{
    if (any(theta == 0)){
      stop("For the Frank Copula, the Theta parameter has to be unequal to zero,
           which is currently not the case. Please transform the parameters
           accordingly.")
    }else{
    c <- ((exp(theta) - 1) *theta * exp((theta * (cu + cv + 1))))/
      (exp((theta * (cu + cv))) - exp(theta * cu +theta) -
         exp(theta * cv +theta) + exp(theta))^2
  }}}

  # New Copula:
  # Gauss Copula:
  if (copula=="Gaussian"){
    if (is.vector(theta)==FALSE){if(ncol(theta)>1){
      stop("For the Gaussian Copula only one Theta parameter can be supported per
           observation.")
    }}else{
    if (any(theta>1) |any( theta< -1)){
      stop("For the Gaussian copula, the theta parameter is the correlation
           which is defined between -1 and 1. Please transform the parameters
           accordingly.")
    }else{
      c <- (1/sqrt(1-theta^2))*exp(
        (theta^2*(data[,1]^2+data[,2]^2)-2*theta*data[,1]*data[,2])/(
          2*(1-theta^2)))
    }}}


    if (copula != "Frank" & copula != "Gumbel" & copula != "Clayton"
        & copula != "Gaussian"){
      stop("This copula is not implmented within the function. Please
           construct the copula as an indvidual function. ")
    }

  }else{
    # Copula as function
    if (is.function(copula)==TRUE){
      c <-copula(data1=data[,1],data2=data[,2],theta=theta,
                 ddist1=u,pdist1=cu,ddist2=v,pdist2=cv)

    }else{stop("This form of Copula input is not supported.")}
  }
   if (length(c)!=length(u)){stop("Problem with the Copula function: The copula
                                  does not return for each observation a
                                  corresponding copula density. ")}
  ###################
  # Calculation of likelihood
  ###################

  r <- sum(log(u * v * c))
  out <- list (Cdensity=c,Joint_density=(u * v * c),Likelihood= r)
  # Output return:
  return(out)
}
