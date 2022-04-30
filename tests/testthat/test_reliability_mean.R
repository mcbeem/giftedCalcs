context("reliability_mean()")

expect_equal(reliability_mean(rely=c(.95, .95, .95),
                              r=matrix(c( 1,.4,.7,
                                  .4, 1,.9,
                                  .7,.9, 1), 3,3, byrow=T)),
             0.9785714, tol=1e-5)

expect_error(reliability_mean(rely=c(.8, .8, .8),
                              r=matrix(c( 1,.4,.7,
                                          .4, 1,.9,
                                          .7,.9, 1), 3,3, byrow=T)))


expect_error(reliability_mean(rely=c(.96, .96, .96),
                              r=matrix(c( 1,.4,.7,
                                          .4, 1,.9,
                                          .7,.9, 1), 3,3, byrow=T),
                              w=c(-1, 1, 1)))

