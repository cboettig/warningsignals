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



plot_dists <- function(objects, ...){
  n <- length(objects)
  par(mfrow=c(1,n))
  for(i in 1:n){
    plot(objects[[i]], ...)
  }
}


## plots at increasing effort
for(i in 1:length(freq)){
  file <- paste("ibm_crit_", freq[i], ".png", sep="")
  png(file); 
  plot_roc_curves(c(sampling[i], indicator_sampling[[i]])); 
  dev.off()
  upload(file, script=script, gitaddr=gitaddr, tags=tags)

  file <- paste("dist_ibm_crit_", freq[i], ".png", sep="")
  png(file)
  plot_dists(c(sampling[i], indicator_sampling[[i]])); 
  dev.off()
  upload(file, script=script, gitaddr=gitaddr, tags=tags)

}


