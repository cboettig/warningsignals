# analysis.R
require(warningsignals)
require(socialR)
script <- "caco3_analysis.R"
gitcommit(script)
tags="warningsignals, stochpop"
tweet_errors(script, tags=tags)
on.exit(system("git push")) 

cpu <- 16
nboot <- 160
freq <- c(.1, .5, 1.5, 2, 5)

source("analysis.R")

data(CaCO3) 
caco3 <- analysis(CaCO3)
save(list="caco3", file="caco3_analysis.rda")
png("caco3_roc.png"); compare_roc_curves(caco3$taus, caco3$mc); dev.off()
upload("caco3_roc.png", script=script, tags=tags)
png("caco3_sampling.png"); plot_sampling_freq(caco3$sampling, caco3$freq); dev.off()
upload("caco3_sampling.png", script=script, tags=tags)



