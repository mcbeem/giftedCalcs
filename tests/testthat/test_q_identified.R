
expect_equal(q_identified(relyt=.9, percentile=.9, test.cutoff=.9,
                          nom.cutoff=.9, valid=.5, mu=0),
             2.551956, tolerance=1e-5)

expect_equal(q_identified(relyt=.9, percentile=.7, test.cutoff=.9,
                          nom.cutoff=.9, valid=.5, mu=0),
             2.062849, tolerance=1e-5)

p <- seq(.1, .9, length.out=4)
expect_equal(sapply(p, q_identified, relyt=.9,
                    test.cutoff=.9),
             c(1.07550164336083, 1.45811469749269, 1.7781575353618, 2.32620551433453),
             tolerance=1e-7)

expect_equal(q_identified(relyt=.9, percentile=.7, test.cutoff=.9,
                          nom.cutoff=.9, valid=.5, mu=-1),
             1.788498, tolerance=1e-5)

expect_error(q_identified(relyt=.9, percentile=.7, test.cutoff=.9,
                          nom.cutoff=.9, valid=.99, mu=0))

expect_error(q_identified(relyt=.9, test.cutoff=.9, blarg=4))

expect_warning(q_identified(relyt=.9, percentile=.7, test.cutoff=.9,
                          nom.cutoff=.9, mu=-1))

expect_warning(q_identified(relyt=.9, percentile=.7, test.cutoff=.9,
                          valid=.6, mu=-1))

expect_error(q_identified(relyt=.9, percentile=.7, test.cutoff=1,
                          valid=.6, mu=-1))

expect_warning(q_identified(relyt=.9, percentile=.7, test.cutoff=.9,
                            valid=.6, nom.cutoff=.5, mu=3))


