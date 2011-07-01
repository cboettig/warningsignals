# replot.R
rm(list=ls())
require(warningsignals)

load("~/flickr/5889474134.Rdat")
###############
require(socialR)
script <- "replot.R"
gitaddr <- gitcommit(script)
tags="warningsignals, stochpop"
tweet_errors(script, tags=tags)
###############

source("analysis.R")

## plots at each sampling level

for(i in 1:length(freq)){
  input <- c(sampling[i], indicator_sampling[[i]])
  file <- paste("ibm_crit_", freq[i], ".png", sep="")
  png(file); 
  plot_roc_curves(input, cex.axis=3, cex.lab=3); 
  dev.off()
  upload(file, script=script, gitaddr=gitaddr, tags=tags)

  file <- paste("dist_ibm_crit_", freq[i], ".png", sep="")
  png(file, width=480*length(input))
  plot_dists(input, cex.axis=3, cex.lab=3); 
  dev.off()
  upload(file, script=script, gitaddr=gitaddr, tags=tags)
}


