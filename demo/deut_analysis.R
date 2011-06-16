# analysis.R
rm(list=ls())
require(warningsignals)
require(socialR)
script <- "deut_analysis.R"
gitcommit(script)
tags="warningsignals, stochpop"
tweet_errors(script, tags=tags)
on.exit(system("git push")) 

cpu <- 16
nboot <- 160
freq <- c(.1, .5, 1.5, 2, 5)

source("analysis.R")
data(deuterium)
deut <- vector("list", length=3)
for(i in 1:length(deut)){
  deut[[i]] <- analysis(deuterium[[i]], cpu=cpu, nboot=nboot, freq=freq)
  png("deut[[i]]_roc.png"); compare_roc_curves(deut[[i]]$taus, deut[[i]]$mc); dev.off()
  upload("deut[[i]]_roc.png", script=script, tags=tags)
  png("deut[[i]]_sampling.png"); plot_sampling_freq(deut[[i]]$sampling, deut[[i]]$freq); dev.off()
  upload("deut[[i]]_sampling.png", script=script, tags=tags)
}

save(list="deut", file="deut_analysis.rda")

