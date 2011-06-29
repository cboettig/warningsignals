#ibm_sampling_tau.R

require(warningsignals)


source("../R/bootstrap_indicators.R")



require(socialR)
script <- "ibm_sampling_tau.R"
gitaddr <- gitcommit(script)
tags="warningsignals, stochpop"
tweet_errors(script, tags=tags)
on.exit(system("git push")) 

cpu <- 16
nboot <- 2
freq <- c(.5, 2) #c(.1, .5, 1.5, 2, 5)

source("analysis.R")


data(ibms)
m <- fit_models(ibm_critical, "LSN")
sampling <- indicator_sampling_freq(m, cpu, nboot, sample_effort=freq,
                               length.original=length(m$X)) 

save(list=ls(), file="ibm_sampling_tau.png")


png("tau_sampling.png")
plot_sampling_freq(sampling, freq)
dev.off()
upload("tau_sampling.png", script=script, gitaddr=gitaddr, tags=tags)


