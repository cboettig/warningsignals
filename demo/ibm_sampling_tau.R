#ibm_sampling_tau.R

require(warningsignals)


require(socialR)
script <- "ibm_sampling_tau.R"
gitaddr <- gitcommit(script)
tags="warningsignals, stochpop"
tweet_errors(script, tags=tags)
on.exit(system("git push")) 

cpu <- 4
nboot <- 100
freq <- c(1, 2, 5, 8) 

source("analysis.R")


plot_tau_sampling_freq <- function(sampling_all, freq){
  sampling_vars <- lapply(1:length(sampling_all), function(i) sampling_all[[i]][[1]])
  sampling <- reformat_tau_dists(sampling_vars)

  auc <- numeric(length(sampling))
  auc[1] <- roc_curve(sampling[[1]], lwd=2, col=1)
  legend_txt <- character(length(sampling))
  legend_txt[1] <- paste("Effort", freq[1], "AUC =",round(auc[1],3))
  for(i in 2:length(sampling)){
    auc[i] <- roc_curve(sampling[[i]], lwd=2, col=i, add=TRUE)
    legend_txt[i] <- paste("Effort", freq[i], "AUC =",round(auc[i],3))
  }
  legend("bottomright",legend_txt, col=c(1:length(sampling)), lty=1, lwd=3) 
}


data(ibms)
m <- fit_models(ibm_critical, "LSN")

if(m$timedep$pars[['m']] > 0)
  stop("m > 0")

sampling <- indicator_sampling_freq(m, cpu, nboot, sample_effort=freq,
                               length.original=length(m$X)) 
save(list=ls(), file="ibm_sampling_tau.Rdat")
png("tau_sampling.png")
plot_tau_sampling_freq(sampling, freq)
dev.off()
upload("tau_sampling.png", script=script, gitaddr=gitaddr, tags=tags)


for(i in 1:length(sampling)){
  png("dists.png")
  plot(sampling[[i]])
  dev.off()
  upload("dists.png", script=script, gitaddr=gitaddr, tags=tags)
}


