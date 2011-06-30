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

cpu <- 16
nboot <- 500
freq <- c(.3, .5, .7, .9)
freq_indicator <- c(1, 2, 5, 10)

source("analysis.R")
data(deuterium)

i <- 3

m <- fit_models(deuterium[[i]], "LSN")
#sampling <- sampling_freq(m$const, m$timedep, cpu=cpu, nboot=nboot,
#                          sample_effort=freq, length.original=length(m$X))
#taus <- bootstrap_tau(m$X, m$const, m$timedep, cpu=cpu, nboot=nboot)
#mc <- montecarlotest(m$const, m$timedep, cpu=cpu, nboot=nboot)
indicator_sampling <- indicator_sampling_freq(m, cpu, nboot,
                                              sample_effort=freq_indicator,
                                              length.original=length(m$X)) 
save(list=ls(), file="deut_analysis.rda")




#png(paste("deut_", i, "_roc.png", sep="")); compare_roc_curves(taus, mc); dev.off()
#upload(paste("deut_", i, "_roc.png", sep=""), script=script, gitaddr=gitaddr, tags=tags)


#png(paste("deut_", i, "_sampling.png", sep="")); plot_sampling_freq(sampling, freq); dev.off()
#upload(paste("deut_", i, "_sampling.png", sep=""), script=script, gitaddr=gitaddr, tags=tags)


png("tau_sampling.png"); # variance (stat=1) at different sampling efforts
plot_tau_sampling_freq(indicator_sampling, freq, pts=100, stat=1); 
dev.off()
upload("tau_sampling.png", script=script, gitaddr=gitaddr, tags=tags)



