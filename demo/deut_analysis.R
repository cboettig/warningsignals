# deut_analysis.R
rm(list=ls())
require(warningsignals)

###############
require(socialR)
script <- "deut_analysis.R"
gitaddr <- gitcommit(script)
tags="warningsignals, stochpop"
tweet_errors(script, tags=tags)
###############

cpu <- 16
nboot <- 500
freq <- c(.1, .3, .5, .7, .9)

source("analysis.R")
data(deuterium)
deut <- vector("list", length=3)
for(i in 2){
  deut[[i]] <- analysis(deuterium[[i]], cpu=cpu, nboot=nboot, freq=freq)
  png(paste("deut_", i, "_roc.png", sep="")); 
  compare_roc_curves(deut[[i]]$taus, deut[[i]]$mc); dev.off()
  upload(paste("deut_", i, "_roc.png", sep=""), script=script, gitaddr=gitaddr, tags=tags)
  png(paste("deut_", i, "_sampling.png", sep="")); plot_sampling_freq(deut[[i]]$sampling, deut[[i]]$freq); dev.off()
  upload(paste("deut_", i, "_sampling.png", sep=""), script=script, gitaddr=gitaddr, tags=tags)
}

save(list="deut", file="deut_analysis.rda")

