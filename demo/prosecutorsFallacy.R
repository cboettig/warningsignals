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

# Takes some time, just load the cached simulation data instead
load("prosecutor.rda")

## Find out which ones crashed
d <- dim(sn$x1)
crashed <- which(sn$x1[d[1],]==0)


## Let's get tidy data 
require(reshape2)
require(plyr)
dat <- melt(sn$x1)
names(dat) = c("time", "reps", "value")
dat <- cbind(dat, crashed=dat$rep %in% crashed)
dat <- subset(dat, value!=0)
dat <- arrange(dat, crashed)

require(ggplot2)


#ggplot(dat) + geom_line(aes(time, value, group=reps), alpha=.05) + facet_wrap(~crashed)

## What do the variance trends look like for the crashed data?
crashed_dat <- subset(dat, crashed==TRUE)[c("time", "reps", "value")]
#acorr <- summary_statistic(dat, window_autocorr)
var <- dlply(crashed_dat, "reps", function(X) window_var(X$value))
tmp <- melt(var)
ggplot(tmp) + geom_line(aes(1:length(value), value)) + facet_wrap(~ L1) +
  opts(title="Variance in replicates conditioned on the crash")



## Need heavy parallelization to handle these estimates over the replicates
## see prosecutorsFallacy_modelfits.R
load("prosecutor_mpi_full.rda")


#require(earlywarning)
#models <- dlply(dat, "rep", function(X){
#  Y <- data.frame(X$time, X$population)
#  stability_model(Y, "LSN")
#})
#save("models", file="lsn_models.rda")

indicators <- ddply(dat, "reps", function(X){ 
  Y <- data.frame(X$time, X$value)
  tau <- compute_tau(Y, "Var")[1]
  i <- X$rep[1]
  m <- models[[i]]$pars["m"]
  # the data for a given replicate has n entries with same value for rep
  # and n for the same value of crashed, so just grab the first value
  c(tau, m, crashed=X$crashed[1])
})

require(beanplot)
png("beanplot.png", width=480*2)
par(mfrow=c(1,2))
beanplot(m ~ crashed, indicators, what=c(0,1,0,0))
beanplot(kendall_coef ~ crashed, subset(indicators, abs(m) <1), what=c(0,1,0,0))
dev.off()


ggplot(indicators) + geom_density(aes(kendall_coef, fill=crashed), alpha=.7)
ggplot(indicators) + geom_density(aes(m, fill=crashed), alpha=.7)

## The elegant, fast, data-table way 
require(data.table)
DT <- data.table(dat)
myf <- function(x,y) compute_tau(data.frame(x,y), "Var")[1]
DT[, myf(time, population), by="rep"]







