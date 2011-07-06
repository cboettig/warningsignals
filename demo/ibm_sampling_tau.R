#ibm_sampling_tau.R
rm(list=ls())
require(warningsignals)

############
require(socialR)
script <- "ibm_sampling_tau.R"
gitaddr <- gitcommit(script)
tags="warningsignals, stochpop"
tweet_errors(script, tags=tags)
on.exit(system("git push")) 
############

source("analysis.R")
freq <- c(1, 5, 10, 20)
cpu <- 16
nboot <- 200

data(ibms)
m <- fit_models(ibm_critical, "LSN")

indicator_sampling <- indicator_sampling_freq(m, cpu, nboot,
                                    sample_effort=freq,
                                    length.original=length(m$X)) 

save(list=ls(), file="ibm_sampling.Rdat")

for(i in 1:length(freq)){
  input <- c(indicator_sampling[[i]])
  file <- paste("tau_", freq[i], ".png", sep="")
  png(file); 
  plot_roc_curves(input, cex.axis=2, cex.lab=2); 
  dev.off()
  upload(file, script=script, gitaddr=gitaddr, tags=tags)
}

