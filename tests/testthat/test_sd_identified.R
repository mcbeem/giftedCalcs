expect_equal(sd_identified(relyt=.9, test.cutoff=.000000001), 1, tolerance=1e-5)

expect_equal(sd_identified(relyt=.9, test.cutoff=.9), 0.5022167, tolerance=1e-5)

expect_equal(sd_identified(relyt=.9, valid=.8, test.cutoff=.9, nom.cutoff=.5, mu=0),
             0.4987185, tolerance=1e-5)

expect_equal(sd_identified(relyt=.9, valid=.8, test.cutoff=.9, nom.cutoff=.9, mu=0),
             0.4941329, tolerance=1e-5)

expect_equal(sd_identified(relyt=.9, valid=.8, test.cutoff=.9, nom.cutoff=.5, mu=-1),
             0.4328967, tolerance=1e-5)

expect_error(sd_identified(relyt=1, test.cutoff=.9))

expect_error(sd_identified(relyt=.9, test.cutoff=0))

expect_error(sd_identified(relyt=.9, valid=.99, test.cutoff=.9, nom.cutoff=.5, mu=0))

expect_error(sd_identified(relyt=.9, valid=.8, test.cutoff=.9, mu=0))

expect_warning(sd_identified(relyt=.9, valid=.8, test.cutoff=.9, nom.cutoff=.5, mu=-3))


