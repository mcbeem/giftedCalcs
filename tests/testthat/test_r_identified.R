
set.seed(1)

x <- r_identified(n=150000, relyt=.9, valid=.6,
             test.cutoff=.9, nom.cutoff=.9, mu=0)
expect_equal(mean(x), 1.841753, tolerance=1e-2)

x <- r_identified(n=150000, relyt=.9, test.cutoff=.9, mu=0)
expect_equal(mean(x), 1.651817, tolerance=1e-2)

expect_error(r_identified(relyt=.9, test.cutoff=.9, blarg=4))

expect_error(r_identified(relyt=.9, n=1, test.cutoff=.9,
                          nom.cutoff=.9, mu=-1))

expect_error(r_identified(relyt=.9, n=1, test.cutoff=.9,
                          valid=.6, mu=-1))

expect_error(r_identified(relyt=.9, n=1, test.cutoff=1,
                          valid=.6, mu=-1))

expect_warning(
  r_identified(relyt=.9, n=1, test.cutoff=.9, valid=.6, nom.cutoff=.5, mu=2.9)
)

