# one-stage system
expect_equal(
 as.numeric(unlist(marginal_psychometrics(mu=0, test.cutoff=.9, relyt=.95))),
  c(0.84265963, 0.15734037, 0.09999959), tolerance=1e-5)

# two-stage system
expect_equal(as.numeric(unlist(marginal_psychometrics(mu=0, test.cutoff=.9,
                                          nom.cutoff=.8, relyt=.95, valid=.7))),
  c(0.614016755901868, 0.110109881533096, 0.2, 0.344994485315206,
    0.0689987746280109), tolerance=1e-5)

expect_equal(as.numeric(unlist(marginal_psychometrics(mu=1, test.cutoff=.9,
                                                      nom.cutoff=.8, relyt=.95, valid=.7))),
             c(0.704107780368156, 0.0503800919856875, 0.562920827733536, 0.639919358971327,
               0.28850040995344), tolerance=1e-5)

expect_error(marginal_psychometrics(mu=1, test.cutoff=.9,
                                    relyt=.95, valid=.7))

expect_error(marginal_psychometrics(mu=1, test.cutoff=.9,
                                    relyt=.95, valid=.99, nom.cutoff=.7))

expect_error(marginal_psychometrics(mu=1, test.cutoff=.9,
                                    relyt=.95, nom.cutoff=.7))

expect_error(marginal_psychometrics(mu=1, test.cutoff=.9,
                                    relyt=.95, nom.cutoff=.7, blarg=4))

expect_warning(marginal_psychometrics(mu=10, test.cutoff=.9,
                                    relyt=.95, nom.cutoff=.7, valid=.5))

