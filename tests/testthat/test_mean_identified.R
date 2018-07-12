expect_equal(mean_identified(relyt=.9, test.cutoff=.000000001), 0, tolerance=1e-5)

expect_equal(mean_identified(relyt=.9, test.cutoff=.9), 1.664923, tolerance=1e-5)

expect_equal(mean_identified(relyt=.9, valid=.8, test.cutoff=.9, nom.cutoff=.5, mu=0),
             1.674938, tolerance=1e-5)

expect_equal(mean_identified(relyt=.9, valid=.8, test.cutoff=.9, nom.cutoff=.9, mu=0),
             1.871068, tolerance=1e-5)

expect_equal(mean_identified(relyt=.9, valid=.8, test.cutoff=.9, nom.cutoff=.5, mu=-1),
             1.46008, tolerance=1e-5)

expect_error(mean_identified(relyt=1, test.cutoff=.9))

expect_error(mean_identified(relyt=.9, test.cutoff=0))

expect_error(mean_identified(relyt=.9, valid=.99, test.cutoff=.9, nom.cutoff=.5, mu=0))

expect_error(mean_identified(relyt=.9, valid=.8, test.cutoff=.9, mu=0))

expect_warning(mean_identified(relyt=.9, valid=.8, test.cutoff=.9, nom.cutoff=.5, mu=-3))


