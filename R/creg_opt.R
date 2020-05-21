#' Optimization function for global_creg
#'
#' @description This function is a short cut for the optimization of the
#' global_creg function. The optimisation is either performed by optim() or by
#' optimParallel() of the optimParallel package.
#'
#' @param creg Creg object from global_creg.
#' @param Z Alternative input for the parameter calcualtion,
#'  if no creg object is provided
#' @param data Alternative data for the optimization of beta factors,
#'  if no creg object is provided
#' @param dist1 If no creg object is provided, the first distribution has to be
#' defined.
#' @param dist2 If no creg object is provided, the second distribution has to be
#' defined.
#' @param copula If no creg object is provided, the copula for the calculation
#' needs to be defined
#' @param startbeta Optional starting value for the betas can be supplied.
#' If no value is provided, the result of the creg object is used. If no creg
#' object is provided either, the starting values are set to 1.
#' @param param_trans If no creg opject is provided, tt is possible to assign
#' a transformation function, which transforms the parameters to a specific
#' format (e.g. circular,binary,...)
#' @param method Method for the optimization. Note that for parallel
#' optimization only the "L-BFGS-B" method is supported. For a single core
#' optimizaiton other options are: "Nelder-Mead", "BFGS", "CG", "SANN","Brent"
#' @param cores Defines the number of cores that are used for the optimization.
#' @param maxit Defines the maximal number of iterations.
#' @param infinity_control Optional parameter. See details
#' @param recalculate Logical parameter, whether the optimal model should be
#' recalculated and returned instead of the optimization results. Default is
#' FALSE
#' @import optimParallel
#'
#'
#' @export
#'
#' @details This is the optimization function, to optimize a creg object from
#' the function global_creg. Within this function both one core optimization
#' with optim() as well as multiple optimization through optimParallel() can
#' be computed.
#'
#'
#' If no creg object has been calculated, it is optionally possible to provide
#' the corresponding variables instead.
#' Note that the format of the function has to be in accordance with the +
#' global_creg definitions.
#'
#' The parameter infinity_control can be used to assign a value, in case the
#' Log-Likelihood of the global_creg function is returned as Inf or -Inf. In
#' case of the "L-BFGS-B" method the infinity_control value is per default
#' 10e50 (or -10e50 for negative Inf respectively).
#' @seealso \code{\link{global_creg}}
#'
#' @return Returns the optimizing results from the optimize function.

creg_opt <- function(creg,Z=NULL,data=NULL,dist1=NULL,dist2=NULL,copula=NULL,
                     startbeta=NULL,
                     param_trans=NULL,method="L-BFGS-B",
                     cores=1,maxit=200000, infinity_control=NULL,
                     recalculate=FALSE){
  # source("R/new_creg.R")
  # source("R/global_creg.R")

  if (is.null(Z) & is.null(data)&is.null(dist1)&is.null(dist2)&
      is.null(copula)&is.null(startbeta)){
    if(class(creg)!="creg"){stop("The creg object has to have the class creg")
    }else{
      Z<-creg$input$Z
      data<-creg$input$data
      dist1<-creg$input$dist1
      dist2<-creg$input$dist2
      copula<-creg$input$copula
      startbeta<-creg$input$beta
      param_trans <- creg$input$param_trans
    }}else{
    if(is.null(Z)|is.null(data)|is.null(dist1)|is.null(dist2)|
       is.null(copula)){
      if(class(creg)=="creg"){
        warning("Next to the creg object additional parameters are provided. For the optimization the creg object is used.")
        Z<-creg$input$Z
        data<-creg$input$data
        dist1<-creg$input$dist1
        dist2<-creg$input$dist2
        copula<-creg$input$copula
        startbeta<-creg$input$beta
        param_trans <- creg$input$param_trans
        }
      else{
        stop("Error: Some of the alternative parameters are NULL and no creg object supplied")
      }
    }
  }



  if(round(cores)==cores & cores>0){
  # Pre selction and defining of optional parameters:
  # Testing for different methods:
  if(is.null(startbeta)){
    param_dim <- c(length(Z[[1]]),length(Z[[2]]),1)

    # Count the length/ncol of the matrices:
    beta_length <-0
    for(j in 1:3){
    for (i in 1:param_dim[j]){
      if (is.vector(Z[[j]][[i]])){
         beta_length <- beta_length+length(Z[[j]][[i]])}else{
           if(is.matrix(Z[[j]][[i]])){beta_length<-beta_length+
             ncol(Z[[j]][[i]])}else{
             stop("A Matrix structure has to be implemented")
           }
         }
        }
    }
    startbeta <- rep(1,beta_length)
      }


  ##############################################################
  # Optimization:
  # Priot to the optimization, we have to rewrite the function,
  # in order to recieve the optimization result. We only want to optimize
  # the printed result


    glob_creg_opt <- function(x, Z,data,dist1,dist2,copula,param_trans,m,
                              infinity_control){
      res <- global_creg(beta=x,Z=Z,data=data,dist1=dist1,dist2=dist2,
                         copula=copula,param_trans=param_trans)
      r <-res$result$Likelihood

      # Infinity control:
      if (is.null(infinity_control)==TRUE){
      if (m=="L-BFGS-B"){
      if (r==Inf) {r <- 10e50} else {
        if(r==-Inf){r <- -10e50}
        }
      }}else{
        if (r==Inf) {r <-infinity_control} else {
          if(r==-Inf){r <- -infinity_control}
          }
      }
      return(r)
  }

    ##############################################################
  # Parallel

  if (cores>1){
    # library(optimParallel)

    if (parallel::detectCores(all.tests = FALSE, logical = TRUE)>= cores){
     if(method=="L-BFGS-B"){


       cl <- parallel::makeCluster(spec=cores)
       parallel_control <- list(cl=cl,forward=FALSE,loginfo=FALSE)
       # setDefaultCluster(cl=cl)
       start_time <- Sys.time()
        out <- optimParallel::optimParallel(
                       par=startbeta,fn=glob_creg_opt,Z=Z,data=data,
                       dist1=dist1,dist2=dist2,
                       infinity_control=infinity_control,
                       copula=copula,param_trans=param_trans,m=method,
                       control = list(maxit = maxit, fnscale=-1),
                       parallel = parallel_control)
       end_time <- Sys.time()
     }else {
       stop("The parallel optimisation is only supported with the L-BFGS-B optimization method.")}
    }else{
      stop("The number of selected cores does not correspond with the number of detected cores.")}


  }else{
    if (cores==1){
      start_time <- Sys.time()
      out <- stats::optim(par=startbeta,fn=glob_creg_opt,Z=Z,data=data,
                   dist1=dist1,dist2=dist2,copula=copula,
                   param_trans=param_trans,m=method,
                   method=method,infinity_control=infinity_control,
                   control = list(maxit = maxit, fnscale=-1))


      end_time <- Sys.time()
    }else{stop("The number of cores is not supported.")}
  }
  # calc_time <- end_time - start_time
  # print(calc_time)
  #
  if (recalculate==FALSE){return(out)
  }else{if (recalculate ==TRUE){

    outnew <- global_creg(beta = out$par,Z = Z, data = data,
                           param_trans=param_trans, dist1 = dist1,
                           dist2 = dist2,copula =copula)

    return(outnew)
      }
    }
  }
  else{stop("Incorrect attribute implemented into the cores-variable")}
  }

### recalculate=FALSE muss noch rein !
