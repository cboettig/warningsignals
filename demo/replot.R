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


png("tau_sampling.png"); # variance (stat=1) at different sampling efforts
plot_tau_sampling_freq(indicator_sampling, freq_indicator, pts=100, stat=1); 
dev.off()
upload("tau_sampling.png", script=script, gitaddr=gitaddr, tags=tags)


