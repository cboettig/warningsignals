rm(list=ls())
## import the saved run results
load("../data/manuscriptData.rda")
require(warningsignals)
require(ggplot2)

##  Each data object corresponds to a different data set.  Each has:
## [[1]] : montecarlotest results
## [[2]] : "Variance" tau_dist_montecarlo results
## [[3]] : "Autocorrelation"
## [[4]] : "Skew"
## [[5]] : "CV"

## Note that the raw data is contained in any model object,
## and the model objects are attached to each fit.  e.g. get original the data with:

ibm[[1]]$label <- "Simulation"
drake[[1]]$label <- "Chemostat"
deut3[[1]]$label <- "GlaciationIII"
plotme <- list(ibm[[1]], drake[[1]], deut3[[1]])
roc_data <- lapply(plotme, 
  function(pow){
  ## Traditional plotting methods ##
#  plot(pow$null$X) # the timeseries object
#  plot(pow) # the S3 method for the distributions
#  roc_curve(pow) # the corresponding ROC curve

  # repeat these plots in ggplot2

  X <- pow$null$X
  rawdata <- data.frame(time=as.numeric(time(X)), state=X@.Data)

  # PLOT the timeseries
  p_raw <- ggplot(rawdata, aes(time, state)) + geom_line()+ 
    opts(title=paste(pow$label, "timeseries data"))


  ## PLOT replicate simulations from each model
  null_reps <- replicate(100, simulate(pow$null))
  test_reps <- replicate(100, simulate(pow$test))
  reps <- melt(list(null=null_reps, test=test_reps))
  names(reps) <- c("times", "replicate", "value", "model")
  p_sims <- ggplot(reps) + geom_line(aes(times, value, group=replicate), alpha=0.05) + 
    facet_wrap(~model) + opts(title="Replicate simulations by model")

  ## PLOT parameter distributions (nope)

  ## PLOT the distributions
  dat <- melt(list(Null=pow$null_dist, Test=pow$test_dist))
  names(dat) <- c("value", "simulation")
  observed <- -2*(loglik(pow$null)-loglik(pow$test))
  p_dist <- ggplot(dat) + geom_density(aes(value, fill=simulation), alpha=.7) + 
  geom_vline(xintercept=observed, lty=2) + 
    opts(title="Likelihood ratio distributions", xlab = "Deviance", ylab="Probability density")

  # plot the roc curve
  rocdat <- roc_data(pow$null_dist, pow$test_dist)

# how about boxplot?
#ggplot(dat) + geom_boxplot(aes(simulation, value)) + geom_hline(yintercept=observed, lty=2)
# how about beanplot?
#beanplot(value ~ simulation, dat, what=c(0,1,0,0))

  ## Save plots
  cairo_pdf(paste(pow$label, "_warningsignal.png", sep=""), height=10, width=7)
#  png(paste(pow$label, "_warningsignal.png", sep=""), height=480*10/7, width=480)
  pushViewport(viewport(layout = grid.layout(3,1)))
  vplayout <- function(x, y) viewport(layout.pos.row = x,
  layout.pos.col = y)
  print(p_raw, vp = vplayout(1, 1))
  print(p_sims, vp = vplayout(2, 1))
  print(p_dist, vp = vplayout(3, 1))
  dev.off()

  rocdat
})



rocdat <- rbind(cbind(roc_data[[1]], data="Simulation"), cbind(roc_data[[2]], data="Chemostat"), cbind(roc_data[[3]], data="Glaciation"))
ggplot(rocdat) + geom_line(aes(FalsePos, TruePos, color=data))
ggsave("rocplot.pdf")

# Consider plotting the replicate simulated trajectories from each process





## Parameters got messed up in the pruning unconverged.  Must re-run :-(
# add the parameter labels back that got dropped
#pow$null_par_dist <- matrix(pow$null_par_dist, ncol=length(getParameters(pow$null)), byrow=T) 
#colnames(pow$null_par_dist) <- names(getParameters(pow$null))
#pow$test_par_dist <- matrix(pow$test_par_dist, ncol=length(getParameters(pow$test)), byrow=T) 
#colnames(pow$test_par_dist) <- names(getParameters(pow$test)) 

## plot parameter distributions side by side
# dat <- list(null=pow$null_par_dist, test=pow$test_par_dist)
# pars <- melt(dat)
# names(pars) <- c("replicate", "parameter", "value", "simulation")
# quants <- cast(pars, parameter ~ simulation, quantile)
# ggplot(pars) + geom_boxplot(aes(simulation, value)) + facet_wrap(~parameter)
# ggplot(pars) + geom_boxplot(aes(parameter, value, fill=simulation))




