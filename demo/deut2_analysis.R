# deut_analysis.R
rm(list=ls())
require(warningsignals)


###############
require(socialR)
script <- "deut2_analysis.R"
gitaddr <- gitcommit(script)
tags="warningsignals, stochpop"
###############
source("analysis.R")
data(deuterium)
i <- 2 ## Which deut?
m <- fit_models(deuterium[[i]], "LSN")

sampling <- sampling_freq(m$const, m$timedep, cpu=cpu, nboot=nboot,
                          sample_effort=freq)
taus <- reformat_tau_dists(bootstrap_tau(m$X, m$const, m$timedep, 
                                         cpu=cpu, nboot=nboot))
mc <- remove_unconverged(montecarlotest(m$const, m$timedep, 
                                        cpu=cpu, nboot=nboot))
indicator_sampling <- indicator_sampling_freq(m, cpu, nboot,
                                              sample_effort=freq) 

save(list=ls(), file="deut2.Rdat")
