# replot.R
rm(list=ls())
require(warningsignals)

#load("~/flickr/5888212714.Rdat")
load("ibm_analysis.Rdat")
###############
require(socialR)
script <- "replot.R"
gitaddr <- gitcommit(script)
tags="warningsignals, stochpop"
tweet_errors(script, tags=tags)
###############



## plots at increasing effort
for(i in 1:length(freq)){
  file <- paste("ibm_crit_", freq[i], ".png", sep="")
  a <- reformat_tau_dists(indicator_sampling[[i]])
  png(file); 
  plot_roc_curves(c(sampling[i],a)); 
  dev.off()
  upload(file, script=script, gitaddr=gitaddr, tags=tags)
}


