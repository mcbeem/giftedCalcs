expect_equal(
  as.numeric(unlist(conditional_moments(relyt=.9, true.score=2))),
  c(1.89736659610103, 0.1), tolerance=1e-7)

expect_equal(
  unlist(as.numeric(conditional_moments(relyt=.9, true.score=0))),
  c(0, .1), tolerance=1e-7)

expect_equal(
  as.numeric(unlist(conditional_moments(relyt=.99999, valid=.99, true.score=0))),
  c(0, 0, 0.0198901989019891, 0, 0, 9.99999999995449e-06), tolerance=1e-7)

expect_error(conditional_moments(relyt=.99999, valid=1, true.score=0))
expect_error(conditional_moments(relyt=.99999, valid=0, true.score=0))
expect_error(conditional_moments(relyt=.1, valid=.9, true.score=0))
expect_error(conditional_moments(relyt=-1, valid=.9, true.score=0))


