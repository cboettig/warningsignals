# replot.R
rm(list=ls())
require(warningsignals)

###############
require(socialR)
script <- "replot.R"
gitaddr <- gitcommit(script)
tags="warningsignals, stochpop"
tweet_errors(script, tags=tags)
###############

load("5905640579.Rdat")
png("all_fits.png", width=2*480)
barplot(mt)
dev.off()
upload("all_fits.png", script=script, gitaddr=gitaddr, tags=tags)

