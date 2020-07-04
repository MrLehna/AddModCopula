set.seed(42)
beta1 <- c(1,1,2,1,0.5,2,1)
library(distr)
# Creating example input:
# First distr. 2 Parameters(one matrix, one vector)
# Second distr. 1 Parameter(one matrix )
Z1 <- list(b1=list(matrix(c(rep(1,8),
                            rep(0.5,4),rep(2,4)),8,2),
                   seq(2,16,length.out = 8)),
           b2=list(matrix(c(seq(1,8,length.out = 8),
                            rnorm(8),rep(1,8)),8,3)),
           theta=list(rep(1,8))
)

dat1 <- matrix(c(rnorm(8,mean=4),seq(1,8)),8,2)


# first example transfer function.
trans1 <- function(x){
  # Only Positive sd is allowed:
  x[,2] <- abs(x[,2])
  # To recieve a positive Lambda and only integer value:
  x[,3] <- round(abs(x[,3]))
  # Theta unequal to zero
  x[,4] <- exp(x[,4])
  return(x)
}

transWRONG <- function(x){
  # Now NO positive SD enforeced!

  # To recieve a positive Lambda and only integer value:
  x[,3] <- round(abs(x[,3]))
  # Theta unequal to zero
  x[,4] <- exp(x[,4])
  return(x)
}
skip_on_cran()
test_that("Testing for optimization:", {
  a<- global_creg(beta=beta1,Z=Z1,data=dat1,
                  dist1=Norm,dist2=Pois,
                  copula="Frank",
                  param_trans=trans1 )
  out <- creg_opt(a,method = "L-BFGS-B")
  out
  expect_equal(round(out$value), -31)
  expect_equal(out$convergence,0)

  # Now wrong transformation function:
  expect_error(creg_opt( global_creg(beta=beta1,Z=Z1,data=dat1,
                                     dist1=Norm,dist2=Pois,
                                     copula="Frank",
                                     param_trans=transWRONG)))

  # Let's see, what happends if we supply some values
  expect_warning(creg_opt(a,method = "L-BFGS-B",maxit = 5,copula="Frank"))
  out2 <- creg_opt(startbeta=beta1,Z=Z1,data=dat1,
                   dist1=Norm,dist2=Pois,
                   copula="Frank",
                   param_trans=trans1)
  expect_equal(out2$value,out$value)

  # Null parameter supplied:
  expect_error(creg_opt(startbeta=beta1,Z=Z1,data=dat1,
                        dist1=NULL,dist2=Pois,
                        copula="Frank",
                        param_trans=trans1))

  # Error if parallel computing is wanted without supplying the correct algorithm
  expect_error(creg_opt(a, method = "BFGS",cores = 4))
  expect_error(creg_opt(a, method = "BFGS",cores = -1))
})
