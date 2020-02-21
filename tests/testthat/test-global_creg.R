set.seed(42)
library(distr)
beta1 <- c(1,1,2,1,0.5,2,1)
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

  return(x/2)
}

test_that("Testing the overall function of global-creg", {
a<- global_creg(beta=beta1,Z=Z1,data=dat1,
                dist1=Norm,dist2=Pois,
                copula="Frank",
                param_trans=NULL)
expect_equal(a$Non_transformed,a$Transformed)

b<- global_creg(beta=beta1,Z=Z1,data=dat1,
                dist1=Norm,dist2=Pois,
                copula="Frank",
                param_trans=trans1)
expect_equal(b$Non_transformed,b$Transformed*2)
expect_equal(round(print(b)),-43)
expect_equal(class(b),"creg")
})
