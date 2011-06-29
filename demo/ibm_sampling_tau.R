#ibm_sampling_tau.R

require(warningsignals)


source("../R/bootstrap_indicators.R")



require(socialR)
script <- "ibm_sampling_tau.R"
gitaddr <- gitcommit(script)
tags="warningsignals, stochpop"
tweet_errors(script, tags=tags)
on.exit(system("git push")) 

cpu <- 4
nboot <- 100
freq <- c(1, 2, 10) #c(.1, .5, 1.5, 2, 5)

source("analysis.R")


plot_sampling_freq <- function(sampling, freq){
  auc <- numeric(length(sampling))
  auc[1] <- roc_curve(sampling[[1]], lwd=2, col=1)
  legend_txt <- character(length(sampling))
  legend_txt[1] <- paste("Effort", freq[1], "AUC =",round(auc[1],3))
  for(i in 2:length(sampling)){
    sampling[[i]] <- remove_unconverged(sampling[[i]])
    auc[i] <- roc_curve(sampling[[i]], lwd=2, col=i, add=TRUE)
    legend_txt[i] <- paste("Effort", freq[i], "AUC =",round(auc[i],3))
  }
  legend("bottomright",legend_txt, col=c(1:length(sampling)), lty=1, lwd=3) 
}



data(ibms)
m <- fit_models(ibm_critical, "LSN")
sampling <- indicator_sampling_freq(m, cpu, nboot, sample_effort=freq,
                               length.original=length(m$X)) 

save(list=ls(), file="ibm_sampling_tau.Rout")


png("tau_sampling.png")
plot_sampling_freq(sampling, freq)
dev.off()
upload("tau_sampling.png", script=script, gitaddr=gitaddr, tags=tags)


