# analysis_plots.R
rm(list=ls())
require(warningsignals)

## some plotting functions
load("manuscript_plotting.R")


## Loads saved data to create the plots, rather then re-running the parametric bootstrap for days
sets <- c(1,2,4)

load("~/flickr/5909491217.Rdat")
deut3_resample <- lapply(sets, function(i) c(sampling[i], indicator_sampling[[i]]))
deut3 <- c(list(mc), taus) 
load("~/flickr/5909610015.Rdat")
caco3_resample <-  lapply(sets, function(i) c(sampling[i], indicator_sampling[[i]]))
caco3 <- c(list(mc), taus) 
load("~/flickr/5910219566.Rdat")
deut1_resample <- lapply(sets, function(i) c(sampling[i], indicator_sampling[[i]]))
deut1 <- c(list(mc), taus) 
load("~/flickr/5910198136.Rdat")
drake_resample <- lapply(sets,function(i) c(sampling[i], indicator_sampling[[i]]))
drake <- c(list(mc), taus)
load("~/flickr/5906482315.Rdat")
ibm_resample <-  lapply(sets, function(i) c(sampling[i], indicator_sampling[[i]]))
ibm <- c(list(mc), taus)

# Group the data into lists
appendix <- list("(a) Greenhouse Earth"=caco3, "(b) Glaciation I"=deut1)
appendix_resample <- list("(a) Greenhouse Earth"=caco3_resample, "(b) Glaciation I"=deut1_resample)
roc_data <- list("(a) Simulation"=ibm, "(b) Daphnia"=drake, "(c) Glaciation III"=deut3)
resample <- list("(a) Simulation"=ibm_resample, "(b) Daphnia"=drake_resample, "(c) Glaciation III"=deut3_resample)

##
roc

## Figures 3 & 4 from the main text
cairo_pdf("Fig3.pdf", width=8, height=3)
roc_fig3(roc_data)
dev.off()

cairo_pdf("Fig4.pdf", width=8, height=8*2/3)
ylab <- c("25 pts", "50 pts", "200 pts")
roc_effort_plot(resample, freq=ylab)
dev.off()


## Figures in the appendix 
png("a3.png", width=8, units="in", height=3*3/2, res=400)
roc_fig3(appendix)
dev.off()
png("a4.png", width=8, units="in", height=8*2/3, res=400)
ylab <- c("25 pts", "50 pts", "200 pts")
roc_effort_plot(appendix_resample, freq=ylab)
dev.off()

# distributions
png("3dists.png", width=6, units="in", height=6, res=400)
dists_fig3(roc_data)
dev.off()

png("a3dists.png", width=6, units="in", height=6, res=400)
dists_fig3(appendix)
dev.off()



