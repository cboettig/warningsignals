# replot.R
rm(list=ls())
require(warningsignals)

## Load must come BEFORE socialR block ##
load("~/flickr/5904550426.Rdat")




############### Use correct tagging for replot file ########
require(socialR)
script <- "replot.R"
gitaddr <- gitcommit(script)
tags="warningsignals, stochpop"
tweet_errors(script, tags=tags)     
###########################################################

freq=500
cpu=16
indicator_sampling <-

windows <- c(5,10,25,50)
lapply(windows, function(w){
  reformat_tau_dists(
    bootstrap_tau(m$X, m$const, m$timedep, cpu, nboot,
                  sample_effort=500, windowsize=w))
})


for(i in 1:length(windows)){
  input <- indicator_sampling[[i]]
  file <- paste("deut3_", windows[i], ".png", sep="")

  png(file); 
  plot_roc_curves(input, cex.axis=2, cex.lab=2); 
  dev.off()
  upload(file, script=script, gitaddr=gitaddr, tags=tags)

}



