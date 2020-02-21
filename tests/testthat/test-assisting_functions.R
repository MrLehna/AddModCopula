test_that("Testing List structure", {
  dist1 <- list(dnorm,pnorm)
  t1 <- global_dist_calc(Y=c(1,-1),param=matrix(c(0,0,1,1),2,2),
                         k=2,dist=dist1)
  expect_equal(t1[1,1], t1[2,1])
  expect_equal(t1[1,2], 1-t1[2,2])
  expect_error(global_dist_calc(Y=c(1,-1),param=matrix(c(1,1,0,0),2,2),
                                k=2,dist=list(dist1,norm(1,1,1))))
  expect_error(global_dist_calc(Y=c(1,-1),param=matrix(c(1,1,0,0),2,2),
                                k=1,dist=list(dist1)))


  })


test_that("Testing Distr. Structure", {
  dist2 <- distr::Norm
  t1 <- global_dist_calc(Y=c(1,-1),param=matrix(c(0,0,1,1),2,2),
                         k=2,dist=dist2)
  expect_equal(t1[1,1], t1[2,1])
  expect_equal(t1[1,2], 1-t1[2,2])

  expect_error(global_dist_calc(Y=c(1,-1),param=matrix(c(0,0,1,1),2,2),
                                k=2,dist=dist::Pois))

  expect_error(global_dist_calc(Y=c(1,-1),param=matrix(c(0,0,1,1),2,2),
                                k=2,dist=dist::Norm()))

  expect_error(global_dist_calc(Y=c(1,-1),param=matrix(c(0,0,1,1),2,2),
                                k=2,dist=qnorm()))

})

test_that("Testing vonMises structure", {
  t1 <- global_dist_calc(Y=c(1,0),param=matrix(c(1,1,2,2),2,2),
                               k=2,dist="vonMises")
  expect_equivalent(round(t1,digits = 2),matrix(c(0.52,0.21,0.39,0.00),2,2))
  expect_error(global_dist_calc(Y=c(1,0),param=matrix(c(1,1,2,2),2,2),
                                k=1,dist="vonMises"))
  expect_error(global_dist_calc(Y=c(1,0),param=matrix(c(1,1,2,2),2,2),
                                k=2,dist="Wrong"))

})

