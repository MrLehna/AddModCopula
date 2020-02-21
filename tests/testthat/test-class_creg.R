# Build Test File:
tf<- list(result=list(Likelihood=42,Joint_density=c(0.5,0.5),
                      Cdensity=c(0.1,0.1)),
          Non_transformed=matrix(c(1,2,3,4),2,2),
          Transformed=matrix(c(1,1,1,1),2,2),
          input=list(copula="Frank",beta=c(1,1),
                     data=matrix(c(1,2,3,3),2,2)))

 attr(tf, "class") <- "creg"
test_that("testing generic functions of creg", {

  expect_equal(methods::isClass("creg"),TRUE)
  expect_equal(class(tf),"creg")
  # Printing:
  expect_equal(print(tf),42)

  # Summary:
  t2 <- summary(tf)
  t3 <-summary(tf,param=TRUE)
  expect_equal(t2[[1]],c(1,1))
  expect_equal(t2[[2]],2)
  expect_equal(t2$`Result Log-Likelihood`,42)
  expect_equal(t3[[5]],matrix(c(1,2,3,4),2,2))
  expect_equal(t3[[6]],matrix(c(1,1,1,1),2,2))
  expect_error(summary(tf,param="Wrong"))
  # Plotting:
  t4 <- plot(tf)
  expect_equal(class(t4),c("gg","ggplot"))
  expect_error(plot(tf,option="Wrong"),regexp="This option is not supported.")

  # AIC
  expect_equal(AIC(tf),-80)
  #BIC
  expect_equal(round(BIC(tf),digits=2),-82.61)
})

