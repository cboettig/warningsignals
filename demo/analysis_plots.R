# analysis_plots.R
rm(list=ls())
require(warningsignals)
## some plotting functions
source("manuscript_plotting.R")
data(manuscriptData)


## (re)-Group the data into lists
appendix <- list("(a) Greenhouse Earth"=caco3, "(b) Glaciation I"=deut1)
appendix_resample <- list("(a) Greenhouse Earth"=caco3_resample, "(b) Glaciation I"=deut1_resample)

roc_data <- list("(a) Simulation"=ibm, "(b) Daphnia"=drake, "(c) Glaciation I"=deut1, "(d) Glaciation III"=deut3)
resample <- list("(a) Simulation"=ibm_resample, "(b) Daphnia"=drake_resample, "(c) Glaciation I"=deut1_resample, "(d) Glaciation III"=deut3_resample)

##  Add some more labels to the resample data
for(i in 1:length(resample))
  names(resample[[i]]) <- c("25 pts", "50 pts", "200 pts")




## Figures 3 & 4 from the main text
cairo_pdf("Fig3.pdf", width=8, height=3)
roc_fig3(roc_data)
dev.off()

cairo_pdf("Fig4.pdf", width=8, height=8*2/3)
ylab <- c("25 pts", "50 pts", "200 pts")
roc_effort_plot(resample, freq=ylab)
dev.off()


# distributions
cairo_pdf("FigS1.pdf", width=7, height=7)
dists_fig3(roc_data, main="")
dev.off()

# resampling distributions
for(i in 1:length(resample)){
  file=paste("FigS", i+1, ".pdf", sep="")
  cairo_pdf(file, width=7, height=7)
  dists_fig3(resample[[i]], main="")
  dev.off()
}

png("a3dists.png", width=6, units="in", height=6, res=400)
dists_fig3(appendix)
dev.off()


## Figures in the appendix 
png("a3.png", width=8, units="in", height=3*3/2, res=400)
roc_fig3(appendix)
dev.off()
png("a4.png", width=8, units="in", height=8*2/3, res=400)
ylab <- c("25 pts", "50 pts", "200 pts")
roc_effort_plot(appendix_resample, freq=ylab)
dev.off()


