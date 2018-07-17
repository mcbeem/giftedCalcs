
expect_equal(d_identified_v(relyt=.9, true.score=1, test.cutoff=.9,
             nom.cutoff=.9, valid=.5, mu=0, normalize=F),  0.006629165, tolerance=1e-5)

expect_equal(d_identified_v(relyt=.9, true.score=1, test.cutoff=.9,
             nom.cutoff=.9, valid=.5, mu=0, normalize=T),  0.2045942, tolerance=1e-5)

Tscores <- seq(0,4, length.out=4)
expect_equal(sapply(Tscores, d_identified_v, relyt=.9,
                      test.cutoff=.9, normalize=F),
     c(1.01025794103603e-05, 0.0785635465789546, 0.0113955358155694,
       0.000133830225764885), tolerance=1e-7)

expect_equal(d_identified_v(relyt=.9, true.score=1, test.cutoff=.9,
                          nom.cutoff=.9, valid=.5, mu=-1, normalize=T),
             0.4146193, tolerance=1e-5)

expect_error(d_identified_v(relyt=.9, true.score=1, test.cutoff=.9,
                          nom.cutoff=.9, valid=.99, mu=0, normalize=T))

expect_error(sapply(Tscores, d_identified_v, relyt=.9,
                    test.cutoff=.9, blarg=4, normalize=F))

expect_warning(d_identified_v(relyt=.9, true.score=1, test.cutoff=.9,
                          nom.cutoff=.9, mu=-1, normalize=T))

expect_warning(d_identified_v(relyt=.9, true.score=1, test.cutoff=.9,
                          valid=.6, mu=-1, normalize=T))

expect_error(d_identified_v(relyt=.9, true.score=1, test.cutoff=1,
                          valid=.6, mu=-1, normalize=T))

expect_warning(d_identified_v(relyt=.9, true.score=1, test.cutoff=.9,
                          valid=.6, nom.cutoff=.5, mu=3, normalize=T))
