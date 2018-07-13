expect_equal(
  conditional_p_id(true.score=1, relyt=.9, test.cutoff=.9),
  0.1462572, tolerance=1e-5)

expect_equal(
  conditional_p_id(true.score=1, relyt=.9, test.cutoff=.9, valid=.4, nom.cutoff=.9),
  0.02508019, tolerance=1e-5)

Tscores <- seq(2,3, length.out=4)
expect_equal(conditional_p_id(true.score=Tscores, relyt=.9,
                              test.cutoff=.9, nom.cutoff=.9, valid=.5),
             c(0.3843284, 0.4749469, 0.5579379, 0.6377769), tolerance=1e-5)

expect_warning(
  conditional_p_id(true.score=1, relyt=.9, test.cutoff=.9, valid=.4))

expect_warning(
  conditional_p_id(true.score=1, relyt=.9, test.cutoff=.9, nom.cutoff=.4))

expect_error(
  conditional_p_id(true.score=1, relyt=.1, test.cutoff=.9, valid=.9, nom.cutoff=.4))

expect_error(
  conditional_p_id(true.score=1, relyt=.9, test.cutoff=.9, valid=.1, nom.cutoff=1))
