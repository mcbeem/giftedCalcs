
# R package installation
#install.packages("devtools")
#devtools::install_github("mcbeem/giftedCalcs")


#library(giftedCalcs)
#library(ggplot2)

relyt = .95
valid=.9
test.cutoff=.9
nom.cutoff=.8

# calculate implied performance
marginal_psychometrics(relyt=relyt, valid=valid, test.cutoff=test.cutoff, 
                       nom.cutoff=nom.cutoff)

# make some data
d3 <- r_identified(n=350, valid=valid,
                                test.cutoff=test.cutoff, 
                                nom.cutoff=nom.cutoff)
  
estimate_performance(x=d3, id.rate=.032, nom.rate=1-nom.cutoff, reps=20)


obs <- seq(0, 4, by=.01)

y <- sapply(X=obs, 
       FUN=d_identified, valid=valid, test.cutoff=t.cutoff, 
       nom.cutoff=nom.cutoff, normalize=T)

d <- data.frame(cbind(obs, y))

ggplot(d, aes(x=(obs*15)+100, y=y)) + geom_line() + 
  geom_ribbon(aes(ymin=0, ymax=y), fill="blue", alpha=.3) +
  coord_cartesian(ylim=c(0, 2))+
  theme_classic() + 
  theme(text=element_text(size=11, family="Times New Roman")) +
  xlab("Observed scores of identified students") + ylab("Density") + 
  ggtitle(paste0("Nomination validity = ", valid, 
                 "; Nomination cutoff = ", nom.cutoff*100, "th %ile"))


