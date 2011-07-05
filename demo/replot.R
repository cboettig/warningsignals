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

mt <- sapply(list(ibm_crit=ibm_crit, deut1=deut1, deut3=deut3, daphnia=daphnia, caco3=caco3, lin_deterior=lin_deterior, lin_const=lin_const), get.mt)



png("all_fits.png", width=2*480)
barplot(mt)
dev.off()
upload("all_fits.png", script=script, gitaddr=gitaddr, tags=tags)

