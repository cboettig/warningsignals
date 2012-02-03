rm(list=ls())
## import the saved run results
load("../data/manuscriptData.rda")
require(warningsignals)
require(ggplot2)
require(reshape2)
require(grid)


##  Each data object corresponds to a different data set.  Each has:
## [[1]] : montecarlotest results
## [[2]] : "Variance" tau_dist_montecarlo results
## [[3]] : "Autocorrelation"
## [[4]] : "Skew"
## [[5]] : "CV"
## Note that the raw data is contained in any model object,
## and the model objects are attached to each fit.  e.g. get original the data with:


####################################################
## Extract and organize the model-based results ####
####################################################
ibm[[1]]$label <- "Simulation"
drake[[1]]$label <- "Chemostat"
deut3[[1]]$label <- "GlaciationIII"
plotme <- list(ibm[[1]], drake[[1]], deut3[[1]])


sapply(plotme, function(pow){
  observed <- -2*(loglik(pow$null)-loglik(pow$test))
})

###################################################
## Plot those results for each data set          ##
###################################################


dataplots <-  function(pow){
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
    opts(title="Likelihood ratio distributions") +
    scale_x_continuous("Deviance") + scale_y_continuous("Probability density")

  # get the data for the roc curve
  rocdat <- roc_data(pow$null_dist, pow$test_dist)

# how about boxplot?
#ggplot(dat) + geom_boxplot(aes(simulation, value)) + geom_hline(yintercept=observed, lty=2)
# how about beanplot?
#beanplot(value ~ simulation, dat, what=c(0,1,0,0))



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



  ## Save plots
#  cairo_pdf(paste(pow$label, "_warningsignal.pdf", sep=""), height=10, width=7)
  png(paste(pow$label, "_warningsignal.png", sep=""), height=480*10/7, width=480)
  pushViewport(viewport(layout = grid.layout(3,1)))
  vplayout <- function(x, y) viewport(layout.pos.row = x,
  layout.pos.col = y)
  print(p_raw, vp = vplayout(1, 1))
  print(p_sims, vp = vplayout(2, 1))
  print(p_dist, vp = vplayout(3, 1))
  dev.off()

  rocdat
})

roc_data <- lapply(plotme, dataplots)

##########################################
## Plot the ROC curve of model fits     ##
##########################################
rocdat <- rbind(cbind(roc_data[[1]], data="Simulation"), 
                cbind(roc_data[[2]], data="Chemostat"), 
                cbind(roc_data[[3]], data="Glaciation"))
p <- ggplot(rocdat) + geom_line(aes(FalsePos, TruePos, color=data), lwd=1) #+ 
#  coord_cartesian(xlim=c(0,1.1), ylim=c(0,1.1)) 
p + scale_x_continuous("False Positive") + scale_y_continuous("True Positive")
ggsave("rocplot.pdf")





##############################################################################
## Plots for the Summary Statistics Approaches                              ##
##############################################################################
rm(list=ls())
## import the saved run results
require(warningsignals)
data(ibms)
data(drake)
data(deuterium)

X <- list("(a) Stable"=ibm_stable, "(b) Deteriorating"=ibm_critical, "(c) Daphnia"=drake_deterior$H6, "(d) Glaciation III"=deuterium[[3]])
cairo_pdf("Fig2.pdf", width=6, height=3)
all_indicators(X, indicators=c("Var", "Autocor"))
dev.off()




##########################################################################
## Plot the ROC curves for each data set comparing summary              ##
##   statistics and model-based method                                  ##
##########################################################################
require(warningsignals)
load("../data/manuscriptData.rda")
roc_data <- list("(a) Simulation"=ibm[1:3], "(b) Daphnia"=drake[1:3], "(b) Glaciation III"=deut3[1:3])
#png("Fig3.png", width=8, height=3, units="in", res=400)
ce <- 1.2


plot_roc_curves <- function(objects, legend=TRUE, cex.legend=1, hide_auc=FALSE, ...){
  # objects a list in which entry contains a fit object with null and
  # test dists for all the indicators.
  stats <- which_statistic(objects)

  auc <- numeric(length(objects)) # store area under curve
  legend_txt <- character(length(objects)) # AUC will go in legend

  auc[1] <- roc_curve(objects[[1]], lty=1, col=1, ...)
  legend_txt[1] <- paste(stats[1], ", ", round(auc[1],2), sep="")
    if(hide_auc)
      legend_txt[1] <- paste(stats[1])
  for(i in 2:length(objects)){
    auc[i] <- roc_curve(objects[[i]], col=i, lty=i, add=TRUE, ...)
    legend_txt[i] <- paste(stats[i], ", ", round(auc[i],2),sep="")
    if(hide_auc)
      legend_txt[i] <- paste(stats[i])
  }
  if(legend)
    legend("bottomright",legend_txt, col=c(1:length(objects)),
           lty=c(1:length(objects)), lwd=3, cex=cex.legend, bty="n")
}
which_statistic <- function(objects){
  sapply(objects, function(x){
    if(is(x, "pow"))
      "Likelihood"
    else if(is(x, "tau_dist_montecarlo"))
        gsub("^(.{8}).*", "\\1",x$signal) ## TRUNCATE name to 7 chars
  })
}

roc_fig3 <- function(input, ...){
  n <- length(input) # 1..i..n datafiles
    par(mfrow=c(1,n))
    for(i in 1:n){ #work across, col pos
     plot_roc_curves(input[[i]], cex.axis=ce, cex.lab=ce, cex.legend=.8,
                     cex.main=ce, legend=TRUE, main=names(input)[i], ...)
    }
}
cairo_pdf("roc_summary_stats.pdf", width=8, height=3)
roc_fig3(roc_data, lwd=2)
dev.off()





names(ibm) <- c("likelihood", "variance", "autocorrelation")
names(drake) <- c("likelihood", "variance", "autocorrelation")
names(deut3) <- c("likelihood", "variance", "autocorrelation")

dat <- lapply(list(ibm=ibm[2:3], chemostat=drake[2:3], Glaciation=deut3[2:3]), function(x){ 
  lapply(x, function(pow) list(Null=pow$null_dist, Test=pow$test_dist, observed = pow$observed))
})
m <- melt(dat)
names(m) <- c("tau", "simulation", "statistic", "data")
p_dist <- ggplot(subset(m, "simulation" != "observed")) + 
  geom_density(aes(tau, fill=simulation), alpha=.5) + 
  # geom_vline(xintercept=observed, lty=2) +
  facet_grid(statistic ~ data) + 
  opts(title="Distributions of tau by summary statistic") +
  scale_y_continuous("Probability density")

ggsave("summary_dists.pdf")

summary_rocdat <- lapply(list(ibm=ibm[2:3], chemostat=drake[2:3], glaciation=deut3[2:3]), function(x){ 
  lapply(x, function(pow) roc_data(pow$null_dist, pow$test_dist))
})

m <- melt(summary_rocdat, id.vars=c("TruePos", "FalsePos", "Threshold"))
names(m) <- c("True_Postive", "False_Positive", "Threshold", "Statistic", "data")
p <- ggplot(m) + geom_line(aes(variable=="FalsePos", variable=="TruePos", color=L1), lwd=1)  + facet_wrap(~L2)


 
p + scale_x_continuous("False Positive") + scale_y_continuous("True Positive")

