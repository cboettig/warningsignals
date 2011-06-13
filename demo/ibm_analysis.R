# analysis.R
require(warningsignals)
require(socialR)
script <- "ibm_analysis.R"
gitcommit(script)
tags="warningsignals, stochpop"
tweet_errors(script, tags=tags)
on.exit(system("git push")) 

cpu <- 16
nboot <- 160
freq <- c(.1, .5, 1.5, 2, 5)

source("analysis.R")


data(ibms)
ibm_crit <- analysis(ibm_critical)
png("ibm_crit_roc.png"); compare_roc_curves(ibm_crit$taus, ibm_crit$mc); dev.off()
upload("ibm_crit_roc.png", script=script, tags=tags)
png("ibm_crit_sampling.png"); plot_sampling_freq(ibm_crit$sampling, ibm_crit$freq); dev.off()
upload("ibm_crit_sampling.png", script=script, tags=tags)

ibm_stab <- analysis(ibm_stable)
save(list=c("ibm_crit", "ibm_stab"), file="ibms_analysis.rda")

png("ibm_stab_roc.png"); compare_roc_curves(ibm_stab$taus, ibm_stab$mc); dev.off()
upload("ibm_stab_roc.png", script=script, tags=tags)
png("ibm_stab_sampling.png"); plot_sampling_freq(ibm_stab$sampling, ibm_stab$freq); dev.off()
upload("ibm_stab_sampling.png", script=script, tags=tags)



