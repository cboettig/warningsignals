# analysis.R
require(warningsignals)
require(socialR)
script <- "ibm_analysis.R"
gitaddr <- gitcommit(script)
tags="warningsignals, stochpop"
tweet_errors(script, tags=tags)
on.exit(system("git push")) 

cpu <- 4
nboot <- 200
freq <- c(.5, 2, 5, 10)

source("analysis.R")

## The analyses -- slow!
data(ibms)
m <- fit_models(ibm_critical, "LSN")
sampling <- sampling_freq(m$const, m$timedep, cpu=cpu, nboot=nboot,
                          sample_effort=freq, length.original=length(m$X))
taus <- bootstrap_tau(m$X, m$const, m$timedep, cpu=cpu, nboot=nboot)
mc <- montecarlotest(m$const, m$timedep, cpu=cpu, nboot=nboot)
indicator_sampling <- indicator_sampling_freq(m, cpu, nboot,
                                              sample_effort=freq,
                                              length.original=length(m$X)) 

### Plot methods

png("ibm_crit_roc.png"); compare_roc_curves(taus, mc); dev.off()
upload("ibm_crit_roc.png", script=script, gitaddr=gitaddr, tags=tags)

png("ibm_crit_sampling.png"); plot_sampling_freq(sampling, freq); dev.off()
upload("ibm_crit_sampling.png", script=script, gitaddr=gitaddr, tags=tags)


png("tau_sampling.png"); # variance at different sampling efforts
plot_tau_sampling_freq(sampling, freq, pts=100, stat=1); 
dev.off()
upload("tau_sampling.png", script=script, gitaddr=gitaddr, tags=tags)



## IBM STABLE MODEL 
#ibm_stab <- analysis(ibm_stable, cpu=cpu, nboot=nboot, freq=freq)
#png("ibm_stab_roc.png"); compare_roc_curves(ibm_stab$taus, ibm_stab$mc); dev.off()
#upload("ibm_stab_roc.png", script=script, gitaddr=gitaddr, tags=tags)
#png("ibm_stab_sampling.png"); plot_sampling_freq(ibm_stab$sampling, ibm_stab$freq); dev.off()
#upload("ibm_stab_sampling.png", script=script, gitaddr=gitaddr, tags=tags)



