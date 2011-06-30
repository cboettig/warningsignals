# ibm_analysis.R
rm(list=ls())
require(warningsignals)
require(socialR)
script <- "ibm_analysis.R"
gitaddr <- gitcommit(script)
tags="warningsignals, stochpop"
tweet_errors(script, tags=tags)
on.exit(system("git push")) 

cpu <- 16
nboot <- 32
#freq <- c(25, 50, 100, 200, 500)
freq <- c(50)

source("analysis.R")

## The analyses -- slow!
data(ibms)
m <- fit_models(ibm_critical, "LSN")
sampling <- sampling_freq(m$const, m$timedep, cpu=cpu, nboot=nboot,
                          sample_effort=freq)
taus <- reformat_tau_dists(bootstrap_tau(m$X, m$const, m$timedep, 
                                         cpu=cpu, nboot=nboot))
mc <- remove_unconverged(montecarlotest(m$const, m$timedep, 
                                        cpu=cpu, nboot=nboot))
indicator_sampling <- indicator_sampling_freq(m, cpu, nboot,
                                              sample_effort=freq) 

save(list=ls(), file="ibm_analysis.Rdat")

### Plot methods

## Original plot
png("ibm_crit_roc.png"); plot_roc_curves(c(mc, taus)); dev.off()
upload("ibm_crit_roc.png", script=script, gitaddr=gitaddr, tags=tags)

## plots at increasing effort
for(i in 1:length(freq)){
  file <- paste("ibm_crit_", freq[i], ".png")
  png(file); plot_roc_curves(c(sampling[[i]], indicator_sampling[[i]])); dev.off()
  upload(file, script=script, gitaddr=gitaddr, tags=tags)
}



## IBM STABLE MODEL 
#ibm_stab <- analysis(ibm_stable, cpu=cpu, nboot=nboot, freq=freq)
#png("ibm_stab_roc.png"); compare_roc_curves(ibm_stab$taus, ibm_stab$mc); dev.off()
#upload("ibm_stab_roc.png", script=script, gitaddr=gitaddr, tags=tags)
#png("ibm_stab_sampling.png"); plot_sampling_freq(ibm_stab$sampling, ibm_stab$freq); dev.off()
#upload("ibm_stab_sampling.png", script=script, gitaddr=gitaddr, tags=tags)



