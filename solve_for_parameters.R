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


n <- 200
reps <- 20
test.cutoff <- .9
relyt <- .95
valid <- .7
nom.cutoff <- .7

results <- matrix(ncol=4, nrow=reps)
for (i in 1:reps) {

  d <- r_identified(n=n, test.cutoff=test.cutoff, relyt=relyt, valid=.7, nom.cutoff=.8)
  id.rate <- marginal_psychometrics(test.cutoff=test.cutoff, relyt=relyt,
                                    valid=.7, nom.cutoff=.8)$identification.rate
  nom.rate <- marginal_psychometrics(test.cutoff=test.cutoff, relyt=relyt,
                                     valid=.7, nom.cutoff=.8)$nom.rate

  dens <- density(d, n=64, from=-1, to=4)

  # search for parameters of mixture distribution using
  #   the Levenburg-Marquardt algorithm

  data <- matrix(cbind(dens$x, dens$y*id.rate), ncol=2)

  parms <- try(summary(nlsLM(data[,2] ~ d_identified_v(true.score=data[,1],
                                                        test.cutoff=pnorm(test.cutoff.z),
                                                        relyt=relyt,
                                                        valid=sqrt(relyt)+a,
                                                        nom.cutoff=1-nom.rate,
                                                        mu=0,
                                                        normalize=F),
                         lower=c(0, .7, -.6),
                         upper=c(3, .99, -.01),
                         start=list(test.cutoff.z=1, relyt=.9, a=-.3),
                         control=nls.lm.control(maxiter=200, nprint=1))))
  print(parms)
  pnorm(parms$coefficients[1:2,1])
  #sqrt(parms$coefficients[1,1])+parms$coefficients[3,1]
  print(i)

  results[i,1:3] <- parms$coefficients[1:3,1]
  results[i,1] <- pnorm(results[i,1])
  results[i,4] <- sqrt(results[i,2])+results[i,3]
  #parms$coefficients[1,1] <- NA
}

#results[,1] <- pnorm(results[,1])

apply(results, 2, mean, na.rm=T)
apply(results, 2, sd, na.rm=T)



f1 <- function(a, ...) {
  print(a)
  f2(...)
}

# Treat f2 as a stand-alone function
f2 <- function(b, c) {
  print(b == TRUE)
  print(runif(c))
}

d=4
f1(a=d, b=FALSE, 2)
