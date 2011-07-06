# analysis_plots.R
rm(list=ls())

load("~/flickr/5910219566.Rdat")


###########################
require(socialR)
script <- "analysis_plots.R"
gitaddr <- gitcommit(script)
tags="warningsignals, stochpop"
tweet_errors(script, tags=tags)
###########################

require(warningsignals)
source("analysis.R")


roc_effort_plot <- function(input, main=main, ...){
# plots as a column
  n <- length(input)

  par(mfrow=c(n,1), mar=c(0,5,0,2), oma=c(4,0,4,0))
    plot_roc_curves(input[[1]], cex.axis=1.5, cex.lab=1.5, cex.main=1.5, legend=F, lwd=4, xaxt="n",main=main, ...)
  for(i in 2:(n-1)){
    plot_roc_curves(input[[i]], cex.axis=1.5, cex.lab=1.5, legend=F, lwd=4, xaxt="n", ...)
  }
  plot_roc_curves(input[[n]], cex.axis=1.5, cex.lab=1.5, legend=F, lwd=4, ...)
}



input <- lapply(1:length(freq),
                function(i) c(sampling[i], indicator_sampling[[i]]))

png("rocs.png", width=2, units="in", height=8, res=400)
roc_effort_plot(input, main="Glaciation I")
dev.off()

upload("rocs.png", script=script, gitaddr=gitaddr, tags=tags)



