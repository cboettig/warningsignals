# analysis.R
require(warningsignals)
require(socialR)
script <- "drake_analysis.R"
gitcommit(script)
tags="warningsignals, stochpop"
tweet_errors(script, tags=tags)
on.exit(system("git push")) 

freq <- c(2, 5, 10, 20, 40)
source("analysis.R")

data(drake)
drake <- analysis(drake_deterior$H6, cpu=8, nboot=1000, freq=freq)
save(list="drake", file="drake_analysis.rda")

png("drake_roc.png"); compare_roc_curves(drake$taus, drake$mc); dev.off()
upload("drake_roc.png", script=script, tags=tags)
png("drake_sampling.png"); plot_sampling_freq(drake$sampling, drake$freq); dev.off()
upload("drake_sampling.png", script=script, tags=tags)


