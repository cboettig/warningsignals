# replot.R
rm(list=ls())
require(warningsignals)

load("5887222271.Rdat")

###############
require(socialR)
script <- "replot.R"
gitaddr <- gitcommit(script)
tags="warningsignals, stochpop"
tweet_errors(script, tags=tags)
###############


png(paste("deut_", i, "_roc.png", sep="")); 
compare_roc_curves(deut[[i]]$taus, deut[[i]]$mc); 
dev.off()
upload(paste("deut_", i, "_roc.png", sep=""), 
       script=script, gitaddr=gitaddr, tags=tags)

