# modelchoice_plot.R
rm(list=ls())
require(warningsignals)


load("ibm_modelchoice.Rdat")

############### Use correct tagging for replot file ########
require(socialR)
script <- "modelchoice_plot.R"
gitaddr <- gitcommit(script)
tags="warningsignals, stochpop"
tweet_errors(script, tags=tags)     
###########################################################

png("modelchoice.png")
plot(mc)
dev.off()
upload("modelchoice.png", script=script, gitaddr=gitaddr, tags=tags, comment="IBM model choice: LTC vs LSN")

