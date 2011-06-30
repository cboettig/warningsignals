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
source("analysis.R")



mc <- remove_unconverged(deut[[1]]$mc)
roc_curve(mc)

taus <- reformat_tau_dists(deut[[1]]$taus) 

png("deut1_roc.png"); plot_roc_curves(mc); dev.off()
upload("deut1_roc.png", script=script, gitaddr=gitaddr, tags=tags)

