context("p_identified()")

expect_equal(p_identified(relyt=.9, true.score=1, test.cutoff=.9,
                          nom.cutoff=.9, valid=.5, mu=0),
             0.03479571, tolerance=1e-5)

expect_equal(p_identified(relyt=.9, true.score=1, test.cutoff=.9,
                          nom.cutoff=.9, valid=.5, mu=0),
             0.03479571, tolerance=1e-5)

expect_equal(p_identified(relyt=.9, true.score=1, test.cutoff=.9,
                          nom.cutoff=.9, valid=.5),
             0.03479571, tolerance=1e-5)

Tscores <- seq(0,4, length.out=4)
expect_equal(sapply(Tscores, p_identified, relyt=.9,
                    test.cutoff=.9),
             c(7.47910103821637e-06, 0.26302976943551, 0.961696480597589,
               0.999683288604609), tolerance=1e-7)

expect_equal(p_identified(relyt=.9, true.score=1, test.cutoff=.9,
                          nom.cutoff=.9, valid=.5, mu=-1),
             0.08221583, tolerance=1e-5)

expect_error(p_identified(relyt=.9, true.score=1, test.cutoff=.9,
                          nom.cutoff=.9, valid=.99, mu=0))

expect_error(p_identified(relyt=.9, test.cutoff=.9, blarg=4))

expect_warning(p_identified(relyt=.9, true.score=1, test.cutoff=.9,
                          nom.cutoff=.9, mu=-1))

expect_warning(p_identified(relyt=.9, true.score=1, test.cutoff=.9,
                          valid=.6, mu=-1))

expect_error(p_identified(relyt=.9, true.score=1, test.cutoff=1,
                          valid=.6, mu=-1))

expect_warning(p_identified(relyt=.9, true.score=1, test.cutoff=.9,
                            valid=.6, nom.cutoff=.5, mu=3))


