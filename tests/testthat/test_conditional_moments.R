# test functions

?expect_equal

expect_equal(
  as.numeric(conditional_moments(relyt=.9, true.score=2)$conditional.mean),
  1.897367, tolerance=.000001)

expect_equal(
  as.numeric(conditional_moments(relyt=.9, true.score=0)$conditional.mean),
  0, tolerance=.000001)

expect_equal(
  as.numeric(conditional_moments(relyt=.99999, valid=.99, true.score=0)$conditional.mean),
  c(0,0), tolerance=.000001)

expect_error(conditional_moments(relyt=.99999, valid=1, true.score=0))
expect_error(conditional_moments(relyt=.99999, valid=0, true.score=0))
expect_error(conditional_moments(relyt=.1, valid=.9, true.score=0))
expect_error(conditional_moments(relyt=-1, valid=.9, true.score=0))


