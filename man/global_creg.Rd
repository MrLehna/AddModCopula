% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/global_creg.R
\name{global_creg}
\alias{global_creg}
\title{Function to calculate the likelihood of a copula with two distributions}
\usage{
global_creg(beta, Z, data, dist1, dist2, copula = "Frank", param_trans = NULL)
}
\arguments{
\item{beta}{Beta parameter for the additive model}

\item{Z}{Underlying data for the additive model. The data has to be provided
in a list format. For more information see Details}

\item{data}{Matrix, containing the unerlying data on which the copula
should be fitted. Should have two collumns and the same row length as
parameters from the additive model.}

\item{dist1}{First distribution, see Details}

\item{dist2}{Second distribution, see Details}

\item{copula}{Defining the copula for the calculation. Can either be "Frank",
"Gumbel", "Gaussian" or "Clayton".}

\item{param_trans}{If wanted, the parameter of the additive model can be
transformed through a function. Note that within the function, the parameters
have to be treated as matrices. For more information see the example function.}
}
\value{
A object of class creg is returned.
}
\description{
This function calculates the likehood of a copula from two
distributions. Prior to the calcualtion, the parameters of both distributions
are calculated through an additive model.
}
\details{
The copula is described by two distinct distributions, which each
have their respective parameters:
\deqn{F_{1,2}(\Gamma_{1},\Gamma_{2} \mid \nu) = C(F_{1}(\Gamma_{1} \mid \nu),
 F_{2}(\Gamma_{2} \mid \nu) \mid \nu)}
Within this package, the underlying parameters of each distribution i.e.
\eqn{\Gamma_{1},\Gamma_{2}} are described through an additive model. Each
\eqn{\Gamma_{i}} can include up to five parameters for the respective distribution
i.e. \eqn{\Gamma_{i}= \gamma_{i1},\gamma_{i2},...\gamma_{i5}}.
Within the model, the coefficients of \eqn{\beta_{i}} describe
the linear relationship with the exogenous data \eqn{Z_{i}}:
\deqn{\eta = \beta_{0} 1_{n} +Z_{1} \beta_{1} + ... +Z_{J}\beta_{J}}
However, in many cases the parameters of the distributions have specific
characteristics, that have to be met. Accordingly, the result of the
additive model have to be applied in order to conduct the right distribtion
and copual:
\deqn{\vartheta_{k}^{(d)} = h_{k}^{(d)}(\eta^{(d)}_{k})}
From this theoretical construct, the density and corresponding likelihood of
a copula can be constructed. For more information see @cite X
Computation:

To conduct the correct calculation, some format requirements have to be
taken into account. First, the provided beta values have to be implemented as
a vector argument. Moreover, the corresponding Z-matrices of each distribution
parameter have to be combined into a list enviroment. This list should have
the following format:\\

\code{Z <- list(parameter1=list(Z1,Z2,Z3,...),parameter2=list(Z1,Z2,Z3,...),
                 theta= c(...))}

Note that Z1,Z2,... are matrices and not data frames.
An additional example can be found below.
Further note that the length of the provided beta vector has to correspond with the
total sum of all collumns provided in the Z argument.
In case of dist1 and dist2, different distributions can be used within this
function. First, all distributions from the package "distr" are supported.
Moreover, individual distributions can be supplied via a list arument.
Here, the first list object has to be the probability p() and the second one the
density d() of the given distribtution. As example:
\code{indiv_dist <- list(pnorm,dnorm)}
Due to the original purpose of this function it is also possible to select
the von Mises distribution seperately by setting dist1="vonMises" or
dist2="vonMises".
For the transformation of the optional transformation of the parameters, a
transformation function has to be provided. This can either be a generic one
such as scale() or an individual function. However, note that the parameter
are in a matrix format. An example can be found below.

Further note, that global_creg returns an S3 class with the name creg.
Accordingly, print(), summary() and plot() functions can be applied.
}
\examples{
# loading data sets
data("zugspitze_synthetic_dataset")


# Defining Z matrices for the parameter estimation:
obs <- length(zugspitze_synthetic_dataset$ws)
ones <- rep(1, obs-9)

# List with lagged data as exogenous variables
Zlist <- list(
 # Weibull
 p1=list(
   scale = cbind(ones, abs(zugspitze_synthetic_dataset$ws[9:(obs-1)])),
   shape_a = cbind(ones, abs(zugspitze_synthetic_dataset$ws[9:(obs-1)]))),
 # von Mises dist
 p2=list(
   mu = cbind(ones, zugspitze_synthetic_dataset$dwd[9:(obs-1)]),
   kappa = cbind(ones, zugspitze_synthetic_dataset$dwd[9:(obs-1)])),
 # Copula
 theta = cbind(ones)
)

# Transfer function
transfct <- function(param){
 param <- exp(param/100)
 param[,3] <- (param[,3]/(param[,3] + 1)) * 2 * pi
 return(param)
}

#Defining remaining structure:
startbeta <- rep(1,9)
dist1 <- list(dweibull,pweibull)
dist2 <-"vonMises"
dat <- cbind(zugspitze_synthetic_dataset$ws[10:obs], zugspitze_synthetic_dataset$wd[10:obs])

# Calculation of creg:
result <- global_creg(beta = startbeta,Z = Zlist,
                     data = dat,param_trans=transfct,
                     dist1 = dist1,dist2 = dist2,copula = "Frank")
print(result)
summary(result)
summary(result,param=TRUE) # Reporting of the distr. parameter

# Get BIC:
AIC(result,BIC=TRUE)
# Possible plotting:
# plot(result)
# Possible optimisation through creg_opt
# opt_res <- creg_opt(result)
}
\seealso{
\code{\link{creg_opt}}
}
