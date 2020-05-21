#' Function to calculate the likelihood of a copula with two distributions
#'
#' @description This function calculates the likehood of a copula from two
#' distributions. Prior to the calcualtion, the parameters of both distributions
#' are calculated through an additive model.
#'
#' @param beta Beta parameter for the additive model
#' @param Z Underlying data for the additive model. The data has to be provided
#' in a list format. For more information see Details
#' @param data Matrix, containing the unerlying data on which the copula
#' should be fitted. Should have two collumns and the same row length as
#' parameters from the additive model.
#' @param dist1 First distribution, see Details
#' @param dist2 Second distribution, see Details
#' @param copula Defining the copula for the calculation. Can either be "Frank",
#' "Gumbel", "Gaussian" or "Clayton".
#' @param param_trans If wanted, the parameter of the additive model can be
#' transformed through a function. Note that within the function, the parameters
#' have to be treated as matrices. For more information see the example function.
#'
#' @details The copula is described by two distinct distributions, which each
#' have their respective parameters:
#' \deqn{F_{1,2}(\Gamma_{1},\Gamma_{2} \mid \nu) = C(F_{1}(\Gamma_{1} \mid \nu),
#'  F_{2}(\Gamma_{2} \mid \nu) \mid \nu)}
#' Within this package, the underlying parameters of each distribution i.e.
#' \eqn{\Gamma_{1},\Gamma_{2}} are described through an additive model. Each
#' \eqn{\Gamma_{i}} can include up to five parameters for the respective distribution
#' i.e. \eqn{\Gamma_{i}= \gamma_{i1},\gamma_{i2},...\gamma_{i5}}.
#' Within the model, the coefficients of \eqn{\beta_{i}} describe
#' the linear relationship with the exogenous data \eqn{Z_{i}}:
#' \deqn{\eta = \beta_{0} 1_{n} +Z_{1} \beta_{1} + ... +Z_{J}\beta_{J}}
#' However, in many cases the parameters of the distributions have specific
#' characteristics, that have to be met. Accordingly, the result of the
#' additive model have to be applied in order to conduct the right distribtion
#' and copual:
#' \deqn{\vartheta_{k}^{(d)} = h_{k}^{(d)}(\eta^{(d)}_{k})}
#' From this theoretical construct, the density and corresponding likelihood of
#' a copula can be constructed. For more information see @cite X
#' Computation:
#'
#' To conduct the correct calculation, some format requirements have to be
#' taken into account. First, the provided beta values have to be implemented as
#' a vector argument. Moreover, the corresponding Z-matrices of each distribution
#' parameter have to be combined into a list enviroment. This list should have
#' the following format:\\
#'
#' \code{Z <- list(parameter1=list(Z1,Z2,Z3,...),parameter2=list(Z1,Z2,Z3,...),
#'                  theta= c(...))}
#'
#' Note that Z1,Z2,... are matrices and not data frames.
#' An additional example can be found below.
#' Further note that the length of the provided beta vector has to correspond with the
#' total sum of all collumns provided in the Z argument.
#' In case of dist1 and dist2, different distributions can be used within this
#' function. First, all distributions from the package "distr" are supported.
#' Moreover, individual distributions can be supplied via a list arument.
#' Here, the first list object has to be the probability p() and the second one the
#' density d() of the given distribtution. As example:
#' \code{indiv_dist <- list(pnorm,dnorm)}
#' Due to the original purpose of this function it is also possible to select
#' the von Mises distribution seperately by setting dist1="vonMises" or
#' dist2="vonMises".
#' For the transformation of the optional transformation of the parameters, a
#' transformation function has to be provided. This can either be a generic one
#' such as scale() or an individual function. However, note that the parameter
#' are in a matrix format. An example can be found below.
#'
#' Further note, that global_creg returns an S3 class with the name creg.
#' Accordingly, print(), summary() and plot() functions can be applied.
#'
#' @seealso \code{\link{creg_opt}}
#'
#' @export
#'
#' @return A object of class creg is returned.
#' @examples
#' # loading data sets
#'data("zugspitze_synthetic_dataset")
#'
#'
#' # Defining Z matrices for the parameter estimation:
#'obs <- length(zugspitze_synthetic_dataset$ws)
#'ones <- rep(1, obs-9)
#'
#'# List with lagged data as exogenous variables
#'Zlist <- list(
#'  # Weibull
#'  p1=list(
#'    scale = cbind(ones, abs(zugspitze_synthetic_dataset$ws[9:(obs-1)])),
#'    shape_a = cbind(ones, abs(zugspitze_synthetic_dataset$ws[9:(obs-1)]))),
#'  # von Mises dist
#'  p2=list(
#'    mu = cbind(ones, zugspitze_synthetic_dataset$dwd[9:(obs-1)]),
#'    kappa = cbind(ones, zugspitze_synthetic_dataset$dwd[9:(obs-1)])),
#'  # Copula
#'  theta = cbind(ones)
#')
#'
#'# Transfer function
#'transfct <- function(param){
#'  param <- exp(param/100)
#'  param[,3] <- (param[,3]/(param[,3] + 1)) * 2 * pi
#'  return(param)
#'}
#'
#' #Defining remaining structure:
#'startbeta <- rep(1,9)
#'dist1 <- list(dweibull,pweibull)
#'dist2 <-"vonMises"
#'dat <- cbind(zugspitze_synthetic_dataset$ws[10:obs], zugspitze_synthetic_dataset$wd[10:obs])
#'
#' # Calculation of creg:
#'result <- global_creg(beta = startbeta,Z = Zlist,
#'                      data = dat,param_trans=transfct,
#'                      dist1 = dist1,dist2 = dist2,copula = "Frank")
#'print(result)
#'summary(result)
#'summary(result,param=TRUE) # Reporting of the distr. parameter
#' # Possible plotting:
#' # plot(result)
#' # Possible optimisation through creg_opt
#' # opt_res <- creg_opt(result)



global_creg <- function(beta,Z,data,dist1,dist2,copula="Frank",param_trans=NULL){


  # source("R/assisting_functions.R")
  # source("R/new_creg.R")
  # source("R/class_creg.R")
  # library(circular)
  # library(distr)




  # collect the dimensional properties of the parameters:
  # If for each parameter a specific list was constructed, this should work:
  param_dim <- c(ifelse(is.list(Z[[1]]),length(Z[[1]]),1),
                 ifelse(is.list(Z[[2]]),length(Z[[2]]),1),
                 ifelse(is.list(Z[[3]]),length(Z[[3]]),1))
  #################
  # In this part the beta and Z are combined to build the parameters for the
  # distributions of the copulas (see eta)

  # Crutial is the specification of the param_dim structure as well as the
  # Input of the Z-List.
  ##################
  # Z*beta =param for all parameters.


  par_length <- sum(param_dim)
  if (is.vector(Z[[1]][[1]])==TRUE){obs <- length(Z[[1]][[1]])}else{
    obs <- nrow(Z[[1]][[1]])}
  # Obtain eta, with the regressions for the untransformed parameter estimates
  param <- matrix(nrow = obs, ncol = par_length)
  param_names <- c()
  # Simple coding for each distr

  # j & k  defines the number of steps in the beta vector!
  # m defines the colum of the param matrix
  j <- 0
  m <- 1
  # 1. Distr:
  for (i in 1:param_dim[1]){
    if (is.vector(Z[[1]][[i]])==TRUE){k <- j + 1
    param[,m] <- (Z[[1]][[i]]) * beta[(j+1):k]
    } else {k<- j +ncol(Z[[1]][[i]])
    param[,m] <- (Z[[1]][[i]]) %*% beta[(j+1):k]
    }
    param_names[m] <- paste("dist1_par",as.character(m),sep="_")
    m <- m +1
    j <- k
  }
  zw <- m-1
  # 2. Distr
  for (i in 1:param_dim[2]){
    if (is.vector(Z[[2]][[i]])==TRUE){
      k <- j + 1
    param[,m] <- Z[[2]][[i]] * beta[(j+1):k]
    } else {
      k<- j +ncol(Z[[2]][[i]])
      param[,m] <- Z[[2]][[i]] %*% beta[(j+1):k]
      }
    param_names[m] <- paste("dist2_par",as.character(m-zw),sep="_")
    m <- m +1
    j <- k
  }
  # Theta

  for (i in 1:param_dim[3]){
    if (is.vector(Z[[3]][[i]])==TRUE){
      k <- j + 1
      param[,m] <- Z[[3]][[i]] * beta[(j+1):k]
    } else {
      k<- j +ncol(Z[[3]][[i]])
      param[,m] <- Z[[3]][[i]] %*% beta[(j+1):k]
    }
    param_names[m] <- paste("Theta parameter",i,sep="_")
    m <- m +1
    j <- k
  }



  #
  #
  # if (is.vector(Z[[3]][[1]])==TRUE){k <- j + 1
  # param[,m] <- Z[[3]][[1]] * beta[(j+1):k]
  # } else {
  #   k<- j +ncol(Z[[3]][[1]])
  #   param[,m] <- Z[[3]][[1]] %*% beta[(j+1):k]
  #   }
  # param_names[m] <- paste("theta")
  if (k!=length(beta)){print(
    "Caution, the length of Beta does not correspond to the amount of Variables
    omitted in the Z list")}


  param <- data.frame(param)
  colnames(param) <- param_names



  #########################################
  # Transformation.
  # If a transformation is wanted, the function is know applied:

  if (is.function(param_trans)){
    # Here it is possible that an error occurs if the wrong structure of the
    # param trans is constructed:
    param_new <- param_trans(param)

  }else{if(is.null(param_trans)){
    param_new <- param
  }else{
    stop("Please provide a function for the transformation")}}



  if (! is.numeric(as.matrix(param_new))){
    stop("Error non numeric parameter estimates.")
  }


  #######################################
  # Calculation of Joint densities:

  # before that, the data set has to be combined:
  if (ncol(data)!=2) {stop("Only for two dimensional data")}

  result <- creg_calc(data=data,param=param_new,dist1=dist1,dist2 = dist2,
            copula = "Frank",param_dim = param_dim)
  out <- list(input=list(beta=beta,Z=Z,data=data,
                         dist1=dist1,dist2=dist2,copula=copula,
                         param_trans=param_trans),
              Non_transformed=param,Transformed=param_new,result=result)


    attr(out, "class") <- "creg"



  return(out)
}


