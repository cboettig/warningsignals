# replot.R
rm(list=ls())
require(warningsignals)

load("~/flickr/5887680595.Rdat")

###############
require(socialR)
script <- "replot.R"
gitaddr <- gitcommit(script)
tags="warningsignals, stochpop"
tweet_errors(script, tags=tags)
###############
source("analysis.R")

mc <- remove_unconverged(mc)
taus <- reformat_tau_dists(taus) 

png("deut1_roc.png"); plot_roc_curves(c(mc, taus)); dev.off()
upload("deut1_roc.png", script=script, gitaddr=gitaddr, tags=tags)

