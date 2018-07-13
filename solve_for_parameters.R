library(minpack.lm)

d <- r_identified(n=500, test.cutoff=.6, relyt=.8)

d <- sort(d)
freqtable <- table(d)

#calculate empirical density
n <- length(d)
cum.dens <- cbind(unique(d),cumsum(freqtable)/n)

dens <- density(d, n=256)

# vectorize the function
d_identified_v <- Vectorize(FUN=d_identified, vectorize.args="true.score")

# search for parameters of mixture distribution using
#   the Levenburg-Marquardt algorithm


data <- matrix(cbind(dens$x, dens$y), ncol=2)

# parms <- summary(nlsLM(cum.dens[,2] ~ p_identified_v(true.score=cum.dens[,1],
#     relyt=relyt,
#     test.cutoff=test.cutoff,
#     valid=valid,
#     nom.cutoff=nom.cutoff,
#     mu=0),
#     lower=c(.95, .5, .5, .001),
#     upper=c(.95, .99, .9, .99),
#     start=list(relyt=.95, test.cutoff=.9, valid=.7, nom.cutoff=.8),
#     control=nls.lm.control(maxiter=200, nprint=1)))


parms <- summary(nlsLM(abs(data[,2]) ~ d_identified_v(true.score=data[,1],
                                                    relyt=relyt,
                                                    test.cutoff=test.cutoff,
                                                    mu=0),
                       lower=c(.5, .5),
                       upper=c(.97, .99),
                       start=list(relyt=.9, test.cutoff=.9),
                       control=nls.lm.control(maxiter=200, nprint=1)))

parms



d_identified_v <- Vectorize(FUN=d_identified, vectorize.args="true.score")
n <- 200
reps <- 20
test.cutoff <- .9
relyt <- .95
valid <- .9
nom.cutoff <- .8

results <- matrix(ncol=4, nrow=reps)
for (i in 1:reps) {

  d <- r_identified(n=n, test.cutoff=test.cutoff, relyt=relyt, valid=valid,
                    nom.cutoff=nom.cutoff)
  id.rate <- marginal_psychometrics(test.cutoff=test.cutoff, relyt=relyt,
                                    valid=valid, nom.cutoff=nom.cutoff)$identification.rate
  nom.rate <- marginal_psychometrics(test.cutoff=test.cutoff, relyt=relyt,
                                     valid=valid, nom.cutoff=nom.cutoff)$nom.rate

  dens <- density(d, n=64, from=-1, to=4, adjust=.5)

  # search for parameters of mixture distribution using
  #   the Levenburg-Marquardt algorithm

  data <- matrix(cbind(dens$x, dens$y*id.rate), ncol=2)

  parms <- try(summary(nlsLM(data[,2] ~ d_identified_v(true.score=data[,1],
                                                        test.cutoff=pnorm(test.cutoff.z),
                                                        relyt=pnorm(relyt.z),
                                                        valid=sqrt(pnorm(relyt.z+a)),
                                                        nom.cutoff=1-nom.rate,
                                                        mu=0,
                                                        normalize=F),
                         lower=c(0, 0, -4),
                         upper=c(3, 3, -.001),
                         start=list(test.cutoff.z=1, relyt.z=1.2, a=-1),
                         control=nls.lm.control(maxiter=200, nprint=1))))
  print(parms)
  print(pnorm(parms$coefficients[2,1]))
  print(i)

  results[i,1:3] <- parms$coefficients[1:3,1]
  results[i,1:2] <- pnorm(parms$coefficients[2,1])
  results[i,4] <- sqrt(pnorm(parms$coefficients[2,1])+parms$coefficients[3,1])


  #parms$coefficients[1,1] <- NA
}

#results[,1] <- pnorm(results[,1])

apply(results, 2, mean, na.rm=T)
apply(results, 2, sd, na.rm=T)








d_identified_v <- Vectorize(FUN=d_identified, vectorize.args="true.score")
n <- 1000
reps <- 100
test.cutoff <- .9
relyt <- .95
valid <- .5
nom.cutoff <- .9

results <- matrix(ncol=4, nrow=reps)
for (i in 1:reps) {

  d <- r_identified(n=n, test.cutoff=test.cutoff, relyt=relyt, valid=valid,
                    nom.cutoff=nom.cutoff)
  id.rate <- marginal_psychometrics(test.cutoff=test.cutoff, relyt=relyt,
                                    valid=valid, nom.cutoff=nom.cutoff)$identification.rate
  nom.rate <- marginal_psychometrics(test.cutoff=test.cutoff, relyt=relyt,
                                     valid=valid, nom.cutoff=nom.cutoff)$nom.rate

  dens <- density(d, n=256, from=.5, to=3.5, adjust=.3, window="t")
  plot(dens)

  # search for parameters of mixture distribution using
  #   the Levenburg-Marquardt algorithm

  data <- matrix(cbind(dens$x, dens$y*id.rate), ncol=2)

  parms <- try(summary(nlsLM(data[,2] ~ d_identified_v(true.score=data[,1],
                                                       test.cutoff=test.cutoff,
                                                       relyt=relyt,
                                                       valid=sqrt(relyt)+a,
                                                       nom.cutoff=1-nom.rate,
                                                       mu=0,
                                                       normalize=F),
                             lower=c(.5, .5, -.7),
                             upper=c(.999, .999, -.001),
                             start=list(test.cutoff=.9, relyt=.9, a=-.2),
                             control=nls.lm.control(maxiter=200, nprint=1))))
  print(parms)
  print(pnorm(parms$coefficients[2,1]))
  print(i)

  results[i,1:3] <- parms$coefficients[1:3,1]
  #results[i,1:2] <- pnorm(parms$coefficients[2,1])
  results[i,4] <- sqrt(parms$coefficients[2,1])+parms$coefficients[3,1]


  #parms$coefficients[1,1] <- NA
}

#results[,1] <- pnorm(results[,1])
#
#
apply(results, 2, mean, na.rm=T)
c(test.cutoff, relyt, NA, valid)
#
apply(results, 2, sd, na.rm=T)
