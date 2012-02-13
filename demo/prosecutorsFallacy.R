##Simulate a dataset from the full individual, nonlinear model
rm(list=ls())
T<- 50000
n_pts <- 1000
require(warningsignals)
# Stable example parameters
pars = c(Xo = 500, e = 0.5, a = 180, K = 1000, h = 200,
    i = 0, Da = 0, Dt = 0, p = 2)
## Run the individual based simulation
#sn <- saddle_node_ibm(pars, times=seq(0,T, length=n_pts), reps=1000)

load("prosecutor.rda")

## Find out which ones crashed
d <- dim(sn$x1)
crashed <- which(sn$x1[d[1],]==0)


## Let's get tidy data 
require(reshape2)
require(plyr)
dat <- melt(sn$x1)
names(dat) = c("time", "rep", "population")
dat <- cbind(dat, crashed=dat$rep %in% crashed)
dat <- subset(dat, population!=0)
dat <- arrange(dat, crashed)

## Let's plot that data
#require(ggplot2)
#ggplot(dat) + geom_line(aes(time, population, group=rep, color=crashed), alpha=.05)

#dat <- subset(dat, rep %in% 1:3 & time < 100)


## Need heavy parallelization to handle these estimates over the replicates
## see prosecutorsFallacy_modelfits.R

require(earlywarning)
models <- dlply(dat, "rep", function(X){
  Y <- data.frame(X$time, X$population)
  stability_model(Y, "LSN")
})

save("models", file="lsn_models.rda")

indicators <- ddply(dat, "rep", function(X){ 
  Y <- data.frame(X$time, X$population)
  tau <- compute_tau(Y, "Var")[1]
  i <- X$rep[1]
  m <- models[[i]]$pars["m"]
  # the data for a given replicate has n entries with same value for rep
  # and n for the same value of crashed, so just grab the first value
  c(tau, m, crashed=X$crashed[1])
})


## WOW, check this out:
require(data.table)
DT <- data.table(dat)
myf <- function(x,y) compute_tau(data.frame(x,y), "Var")[1]
DT[, myf(time, population), by="rep"]





ggplot(tau_var) + geom_density(aes(kendall_coef, fill=crashed))

require(earlywarning)
fits <- lapply(crash_data, function(X){
 stability_model(X, "LSN")
})
m_crash <- sapply(fits, function(x) x$pars["m"])



fits <- lapply(nocrash_data, function(X){
  stability_model(X, "LSN")
})
m_nocrash <- sapply(fits, function(x) x$pars["m"])


# estimate 
