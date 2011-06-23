# analysis.R
require(warningsignals)

##########
require(socialR)
script <- "caco3_analysis.R"
gitaddr <- gitcommit(script)
tags="warningsignals, stochpop"
tweet_errors(script, tags=tags)
on.exit(system("git push")) 
##########

cpu <- 16
nboot <- 16
#freq <- c(.1, .5, 1.5, 2, 5)
freq <- c(.5, 1.5)

source("analysis.R") ## Custom functions

data(CaCO3) 
caco3 <- analysis(CaCO3, cpu=cpu, nboot=nboot, freq=freq)
save(list="caco3", file="caco3_analysis.rda")
png("caco3_roc.png"); compare_roc_curves(caco3$taus, caco3$mc); dev.off()
upload("caco3_roc.png", script=script, gitaddr=gitaddr, tags=tags)
png("caco3_sampling.png"); plot_sampling_freq(caco3$sampling, caco3$freq); dev.off()
upload("caco3_sampling.png", script=script, gitaddr=gitaddr, tags=tags)



