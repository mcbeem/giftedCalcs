library(minpack.lm)
library(scdensity)

d_identified_v <- Vectorize(FUN=d_identified, vectorize.args="true.score")
n <- 5000
reps <- 3
test.cutoff <- .94
relyt <- .92
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

  dens <- scdensity(d, adjust=.5, constraint="unimodal", n=256)#, n=256, from=.5, to=3.5, adjust=.3, window="t")
  #dens <- density(d, n=256, from=.5, to=3.5, adjust=.3, window="t")
  plot(dens, main="")

  # search for parameters of mixture distribution using
  #   the Levenburg-Marquardt algorithm

  data <- matrix(cbind(dens$x, dens$y*id.rate), ncol=2)

  parms <- try(summary(
    nlsLM(data[,2] ~ d_identified_v(true.score=data[,1],
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
  print(i)

  results[i,1:3] <- parms$coefficients[1:3,1]
  #results[i,1:2] <- pnorm(parms$coefficients[2,1])
  results[i,4] <- sqrt(parms$coefficients[2,1])+parms$coefficients[3,1]


  #parms$coefficients[1,1] <- NA
}

#results[,1] <- pnorm(results[,1])
#
colnames(results) <- c("test.cutoff", "relyt", "a", "valid")
apply(results, 2, mean, na.rm=T)[c(1,2,4)]
c(test.cutoff, relyt, NA, valid)
#
# these are simulated empirical SEs
apply(results, 2, sd, na.rm=T)[c(1,2,4)]




estimate_parms <- function(scores, w, id.rate, nom.rate, adjust=.5) {
  # remove missing values
  scores <- na.omit(scores)

  # estimate the density
  #dens <- density(scores[w], n=256, from=.5, to=3.5, adjust=adjust, window="t")
  dens <- scdensity(scores[w], adjust=adjust, constraint="unimodal", n=256)

  # search for parameters of mixture distribution using
  #   the Levenburg-Marquardt algorithm

  data <- matrix(cbind(dens$x, dens$y*id.rate), ncol=2)

  parms <- try(summary(nlsLM(data[,2] ~ d_identified_v(true.score=data[,1],
                                                       test.cutoff=test.cutoff,
                                                       relyt=relyt,
                                                       valid=sqrt(relyt)+valid,
                                                       nom.cutoff=1-nom.rate,
                                                       mu=0,
                                                       normalize=F),
                             lower=c(.5, .5, -.7),
                             upper=c(.999, .999, -.001),
                             start=list(test.cutoff=.9, relyt=.9, valid=-.2),
                             control=nls.lm.control(maxiter=200))))$coef

    results <- parms[,1]
    results[3] <- sqrt(parms[2,1])+parms[3,1]

    return(results)
}

n <- 200
reps <- 10
test.cutoff <- .9
relyt <- .92
valid <- .45
nom.cutoff <- .95

scores <- r_identified(n=n, test.cutoff=test.cutoff, relyt=relyt, valid=valid,
                  nom.cutoff=nom.cutoff)
id.rate <- marginal_psychometrics(test.cutoff=test.cutoff, relyt=relyt,
                                  valid=valid, nom.cutoff=nom.cutoff)$identification.rate
nom.rate <- marginal_psychometrics(test.cutoff=test.cutoff, relyt=relyt,
                                   valid=valid, nom.cutoff=nom.cutoff)$nom.rate

estimate_parms(scores=scores, id.rate=id.rate, nom.rate=nom.rate, adjust=1)

start <- Sys.time()
bootdata <- boot::boot(data=scores, statistic=estimate_parms, R=reps, id.rate=id.rate, nom.rate=nom.rate,
           ncpus=1, parallel="multicore")
Sys.time()-start

bootdata
a <- bootdata$t
#c(test.cutoff, relyt, valid)
#apply(results, 2, mean, na.rm=T)
apply(results, 2, sd, na.rm=T)

start <- Sys.time()
 a <- replicate(n=reps, expr=estimate_parms(scores=sample(scores, size=length(scores), replace=T),
                id.rate=id.rate, nom.rate=nom.rate), simplify="matrix")
Sys.time()-start
 #

# apply(t(a), 1, mean)
# # these are the bootstrapped SEs
# apply(t(a), 1, sd)


par(mfrow=c(3,1))
par(mar=c(2,1,3,3))
plot(density(a[,1], adjust=1.5), xlim=c(0,1), main="test cutoff")
abline(v=test.cutoff, col="red")
plot(density(a[,2], adjust=1.5), xlim=c(0,1), main="test reliability")
abline(v=relyt, col="red")
plot(density(a[,3], adjust=1.5), xlim=c(0,1),main="nomination validity")
abline(v=valid, col="red")

a <- data.frame(a)
names(a) <- c("relyt", "test.cutoff", "valid")


b <- marginal_psychometrics_v(a$relyt, a$test.cutoff, mu=0, valid=a$valid, nom.cutoff=nom.cutoff)
b <- matrix(unlist(b), nrow=reps, ncol=5, byrow=T)

par(mfrow=c(1,2))
par(mar=c(2.5,2.1,3,1))
plot(density(b[,1], adjust=1.5), main="Estimated sensitivity", xlab="")
plot(density(b[,2], adjust=1.5), main="Estimated IIR", xlab="")


# 95% CI for sensitivity
c(round(b[order(b[,1]),1][round(reps*.025,0)],3), round(b[order(b[,1]),1][round(reps*.975,0)],3))


# 95% CI for incorrect identification rate
c(round(b[order(b[,2]),2][round(reps*.025,0)],3), round(b[order(b[,2]),2][round(reps*.975,0)],3))

printit <- function(a,b, reps) {

print(paste0("sensitivity: ", round(mean(b[,1]),3), " +/- ", round(1.96*sd(b[,1]),3),
". 95% CI: [", round(b[order(b[,1]),1][round(reps*.025,0)],3),
     ", ",
     round(b[order(b[,1]),1][round(reps*.975,0)],3), "]"))

  print(paste0("incorrect identification rate: ", round(mean(b[,2]),3), " +/- ", round(1.96*sd(b[,2]),3),
  ". 95% CI: [", round(b[order(b[,2]),2][round(reps*.025,0)],3),
       ", ",
       round(b[order(b[,2]),2][round(reps*.975,0)],3), "]"))
}

printit(a,b,reps)

# real values
marginal_psychometrics_v(relyt=relyt, test.cutoff=test.cutoff, valid, nom.cutoff=nom.cutoff)
