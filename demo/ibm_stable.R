# ibm_stable_analysis.R
rm(list=ls())
require(warningsignals)
#require(socialR)
#script <- "ibm_analysis.R"
#gitaddr <- gitcommit(script)
#tags="warningsignals, stochpop"
#tweet_errors(script, tags=tags)
#on.exit(system("git push")) 


cpu <- 16
nboot <- 500
freq <- c(25, 50, 100, 200, 500)

source("analysis.R")

## The analyses -- slow!
data(ibms)
m <- fit_models(ibm_stable, "LSN")

m$timedep$pars['m'] <- min(m$timedep$pars['m'], 0)


sampling <- sampling_freq(m$const, m$timedep, cpu=cpu, nboot=nboot,
                          sample_effort=freq)
taus <- reformat_tau_dists(bootstrap_tau(m$X, m$const, m$timedep, 
                                         cpu=cpu, nboot=nboot))
mc <- remove_unconverged(montecarlotest(m$const, m$timedep, 
                                        cpu=cpu, nboot=nboot))
indicator_sampling <- indicator_sampling_freq(m, cpu, nboot,
                                              sample_effort=freq) 


save(list=ls(), file="ibm_stable.Rdat")

