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

mc <- deut[[1]]$mc
taus <- deut[[1]]$taus

#mc <- remove_unconverged(deut[[1]]$mc)
#taus <- reformat_tau_dists(deut[[1]]$taus) 

plot(mc)
png("deut1_roc.png"); compare_roc_curves(taus, mc); dev.off()
upload("deut1_roc.png", script=script, gitaddr=gitaddr, tags=tags)

png("deut1_roc.png"); plot_roc_curves(c(mc, taus)); dev.off()
upload("deut1_roc.png", script=script, gitaddr=gitaddr, tags=tags)

