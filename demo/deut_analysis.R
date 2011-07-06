# deut_analysis.R
rm(list=ls())
require(warningsignals)

###############
require(socialR)
script <- "deut_analysis.R"
gitaddr <- gitcommit(script)
tags="warningsignals, stochpop"
tweet_errors(script, tags=tags)
###############
source("analysis.R")

data(deuterium)
i <- 3 ## Which deut?
m <- fit_models(deuterium[[i]], "LSN")


cpu <- 16
nboot <- 16
taus <- 
reformat_tau_dists(
  bootstrap_tau(m$X, m$const, m$timedep, 
                cpu=cpu, nboot=nboot, windowsize=20))

