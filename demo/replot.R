# replot.R
rm(list=ls())
require(warningsignals)

###############
require(socialR)
script <- "replot.R"
gitaddr <- gitcommit(script)
tags="warningsignals, stochpop"
tweet_errors(script, tags=tags)
###############

load("~/flickr/5904550426.Rdat")

cpu=16
out_50 <-  reformat_tau_dists(bootstrap_tau(m$X, m$const, m$timedep, cpu=cpu, 
                                           nboot=nboot, effort=50))


save("replot_50.Rdat")
