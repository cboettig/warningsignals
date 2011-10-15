# ibm_analysis.R
rm(list=ls())
require(warningsignals)
require(socialR)
script <- "ibm_analysis.R"
gitaddr <- gitcommit(script)


cpu <- 16
nboot <- 500
freq <- c(25, 50, 100)

source("analysis.R")

## The analyses -- slow!
load("chaos2.rda")

m <- fit_models(chaos, "LSN")
sampling <- sampling_freq(m$const, m$timedep, cpu=cpu, nboot=nboot,
                          sample_effort=freq)
taus <- reformat_tau_dists(bootstrap_tau(m$X, m$const, m$timedep, 
                                         cpu=cpu, nboot=nboot))
mc <- remove_unconverged(montecarlotest(m$const, m$timedep, 
                                        cpu=cpu, nboot=nboot))
indicator_sampling <- indicator_sampling_freq(m, cpu, nboot,
                                              sample_effort=freq) 

save(list=ls(), file="chaos_analysis.Rdat")


### Plot methods
## Original plot
#png("ibm_crit_roc.png"); plot_roc_curves(c(list(mc), taus)); dev.off()
#upload("ibm_crit_roc.png", script=script, gitaddr=gitaddr, tags=tags)


