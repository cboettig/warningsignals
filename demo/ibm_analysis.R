# analysis.R
require(warningsignals)
require(socialR)
script <- "ibm_analysis.R"
gitaddr <- gitcommit(script)
tags="warningsignals, stochpop"
tweet_errors(script, tags=tags)
on.exit(system("git push")) 

cpu <- 4
nboot <- 100
freq <- c(.1, .5, 1.5, 2, 5)

source("analysis.R")


data(ibms)
m <- fit_models(ibm_critical, "LSN")
sampling <- sampling_freq(m$const, m$timedep, cpu=cpu, nboot=nboot,
                          sample_effort=freq, length.original=length(m$X))
taus <- bootstrap_tau(m$X, m$const, m$timedep, cpu=cpu, nboot=nboot)
mc <- montecarlotest(m$const, m$timedep, cpu=cpu, nboot=nboot)

png("ibm_crit_roc.png"); compare_roc_curves(taus, mc); dev.off()
upload("ibm_crit_roc.png", script=script, gitaddr=gitaddr, tags=tags)

#png("ibm_crit_sampling.png"); plot_sampling_freq(sampling, freq); dev.off()
#upload("ibm_crit_sampling.png", script=script, gitaddr=gitaddr, tags=tags)


## IBM STABLE MODEL 
#ibm_stab <- analysis(ibm_stable, cpu=cpu, nboot=nboot, freq=freq)
#png("ibm_stab_roc.png"); compare_roc_curves(ibm_stab$taus, ibm_stab$mc); dev.off()
#upload("ibm_stab_roc.png", script=script, gitaddr=gitaddr, tags=tags)
#png("ibm_stab_sampling.png"); plot_sampling_freq(ibm_stab$sampling, ibm_stab$freq); dev.off()
#upload("ibm_stab_sampling.png", script=script, gitaddr=gitaddr, tags=tags)



