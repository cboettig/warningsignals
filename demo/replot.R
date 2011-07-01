# replot.R
rm(list=ls())
require(warningsignals)

load("~/flickr/5888212714.Rdat")
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

plot_dists(c(sampling[i], indicator_sampling[[i]])); 

