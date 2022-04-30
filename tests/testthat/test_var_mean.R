context("var_mean()")

expect_equal(var_mean(w=c(1,1,2),
                              r=matrix(c( 1,.4,.7,
                                  .4, 1,.9,
                                  .7,.9, 1), 3,3, byrow=T)),
             0.825, tol=1e-5)

expect_error(var_mean(w=c(-1, 0, 1),
                              r=matrix(c( 1,.4,.7,
                                          .4, 1,.9,
                                          .7,.9, 1), 3,3, byrow=T)))


expect_error(var_mean(r=matrix(c( 1,.4,.7,
                                          .4, 1, 1,
                                          .7,.9, 1), 3,3, byrow=T),
                              w=c(1, 1, 1)))

