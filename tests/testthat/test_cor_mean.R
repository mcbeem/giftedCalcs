context("cor_mean()")

expect_equal(cor_mean(r=matrix(c( 1,.4,.7,
                     .4, 1,.9,
                     .7,.9, 1), 3,3, byrow=T)),
             c(0.7937254, 0.8693183, 0.9827076), tol=1e-5)

expect_error(cor_mean(r=matrix(c( 1,.4,.7,
                                 .4, 1,1.1,
                                 .7,1.1, 1), 3,3, byrow=T)))

expect_error(cor_mean(r=matrix(c( 1, .4, .7,
                                  .4, 1, .9,
                                  .7, .9, 1), 3,3, byrow=T),
                      w=c(-1, 1, 1)))



