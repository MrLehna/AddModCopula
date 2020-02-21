# Define Test data:

data1 <- matrix(c(seq(1,8),rep(1,4),rep(2,4)),8,2)


param1 <- matrix(c(rep(0,8),rep(1,8),seq(1,8),rep(1,8)),8,4)

# One Theta parameter smaller then 1 i.e. 0
param2 <- cbind(param1[,1:3],param1[,4]- c(1,0,0,0,0,0,0,0))

# For gaussian:
param3 <- matrix(c(seq(0,1,length.out=8),rep(1,8),seq(1,8),rep(0.5,8)),8,4)


test_that("Testing Copula functions.", {
  a <- creg_calc(data=data1,param=param1,
                 dist1=Norm,
                 dist2=Pois,
                 copula = "Frank",
                 param_dim = c(2,1,1))
  expect_equal(round(a$Likelihood),-133)
  expect_error(creg_calc(data=data1,param=param1,
                         dist1=Norm,
                         dist2=Pois,
                         copula = "Frank",
                         param_dim = c(2,2,1)))
  expect_error(creg_calc(data=data1,param=param1,
                         dist1=Norm,
                         dist2=Pois,
                         copula = "Frank",
                         param_dim = c(2,1,2)))
  expect_error(creg_calc(data=data1,param=param2,
                         dist1=Norm,
                         dist2=Pois,
                         copula = "Frank",
                         param_dim = c(2,1,1)))
  distlist<- list(dnorm,pnorm)

  b <- creg_calc(data=data1,param=param1,
                 dist1=distlist,
                 dist2=Pois,
                 copula = "Frank",
                 param_dim = c(2,1,1))

  expect_equal(b$Likelihood,a$Likelihood)

  expect_equal(b$Joint_density,a$Joint_density)
  expect_equal(sum(log(b$Joint_density)),b$Likelihood)



  c<- creg_calc(data=data1,param=param1,
               dist1=Norm,
               dist2=Pois,
               copula = "Gumbel",
               param_dim = c(2,1,1))
  expect_equal(round(c$Likelihood,digits = 1),-130.1)
  expect_equal(c$Cdensity,rep(1,8))
  expect_error(creg_calc(data=data1,param=param2,
                         dist1=Norm,
                         dist2=Pois,
                         copula = "Gumbel",
                         param_dim = c(2,1,1)))

  d<- creg_calc(data=data1,param=param1,
               dist1=Norm,
               dist2=Pois,
               copula = "Clayton",
               param_dim = c(2,1,1))
  expect_equal(round(d$Likelihood,digits = 1),-142.5)

  expect_error(creg_calc(data=data1,param=param2-0.1,
                         dist1=Norm,
                         dist2=Pois,
                         copula = "Clayton",
                         param_dim = c(2,1,1)))

  e<- creg_calc(data=data1,param=param1,
                dist1=Norm,
                dist2=Pois,
                copula = "Gaussian",
                param_dim = c(2,1,1))
  # Due to theta= 1 the gaussian copula returns no good values.

  expect_equal(e$Likelihood,NaN)
  expect_equal(e$Cdensity[2] ,Inf)
  f<- creg_calc(data=data1,param=param3,
                dist1=Norm,
                dist2=Pois,
                copula = "Gaussian",
                param_dim = c(2,1,1))
  expect_equal(round(f$Likelihood,digits = 1),-110.4)

  expect_error(creg_calc(data=data1,param=param2+0.1,
                         dist1=Norm,
                         dist2=Pois,
                         copula = "Gaussian",
                         param_dim = c(2,1,1)))

  expect_error(creg_calc(data=data1,param=param2-1.1,
                         dist1=Norm,
                         dist2=Pois,
                         copula = "Gaussian",
                         param_dim = c(2,1,1)))

  })


test_that("Testing Individuall copula", {

indcop <- function(data1,data2,theta,ddist1,pdist1,ddist2,pdist2){
  # Allways returns the c =0.5
  c <- rep(0.5,length(theta))
  return(c)
}
 a <- creg_calc(data=data1,param=param2+0.1,
                dist1=Norm,
                dist2=Pois,
                copula = indcop,
                param_dim = c(2,1,1))
 # Checking, whether the function is executed.
 expect_equal(a$Cdensity,rep(0.5,8))

 # Now implementing a wrong function:

 # 1. Not enough parameter
 indwrong1 <- function(data1,data2){
   # Allways returns the c =0.5
   c <- rep(0.5,length(theta))
   return(c)
 }
 # 2. Returning wrong number of dimension
 indwrong2 <- function(data1,data2,theta,ddist1,pdist1,ddist2,pdist2){
   # Allways returns the c =0.5
   c <- rep(0.5,4)
   return(c)
 }

  # 3. Returns a list
 indwrong3 <- function(data1,data2,theta,ddist1,pdist1,ddist2,pdist2){
   # Allways returns the c =0.5
   c <- rep(0.5,length(theta))
   return(list(c))
 }

 expect_error( creg_calc(data=data1,param=param2+0.1,
                dist1=Norm,
                dist2=Pois,
                copula =  indwrong1,
                param_dim = c(2,1,1)))
 expect_error( creg_calc(data=data1,param=param2+0.1,
                         dist1=Norm,
                         dist2=Pois,
                         copula =  indwrong2,
                         param_dim = c(2,1,1)))
 expect_error( creg_calc(data=data1,param=param2+0.1,
                         dist1=Norm,
                         dist2=Pois,
                         copula =  indwrong3,
                         param_dim = c(2,1,1)))


})

test_that("Testing wrong input", {

  expect_error( creg_calc(data=data1,param=param2+0.1,
                          dist1=Norm,
                          dist2=Pois,
                          copula =  list(),
                          param_dim = c(2,1,1)))
})
