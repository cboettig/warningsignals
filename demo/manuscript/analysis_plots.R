# analysis_plots.R
rm(list=ls())
require(warningsignals)
## some plotting functions

#freq=25,50,100,200,500
sets <- c(1,2,4)

load("ibm_stable3.Rdat")
stable <- c(list(mc), taus)
stable_resample <- lapply(sets, function(i) c(sampling[i], indicator_sampling[[i]]))

load("drake_ltc_G10.Rdat") # doesn't have mc resample, "sampling", and uses only 25, 50, 200
G10 <- c(list(mc), taus)
G10_resample <-  indicator_sampling

data(manuscriptData)
source("manuscript_plotting.R")


save(list=c("stable", "ibm", "drake", "deut3", "deut1", "caco3", "G10",
 "stable_resample", "ibm_resample", "drake_resample", "deut3_resample", "deut1_resample", "caco3_resample", "G10_resample"),
  file="manuscriptData.rda")

## (re)-Group the data into lists
#appendix <- list("(a) Daphnia G10"=G10, "(b) Glaciation I"=deut1)
#appendix_resample <- list("(a) Greenhouse Earth"=caco3_resample, "(b) Glaciation I"=deut1_resample)

roc_data <- list("(a) Stable"=stable, "(b) Deteriorating"=ibm, "(c) Daphnia"=drake, "(d) Glaciation III"=deut3)
resample <- list("(a) Stable" = stable_resample, "(b) Deteriorating"=ibm_resample, "(c) Daphnia"=drake_resample,"(d) Glaciation III"=deut3_resample)

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


