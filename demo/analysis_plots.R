# analysis_plots.R
rm(list=ls())

require(warningsignals)


roc_effort_plot <- function(input, ...){
# plots as a column
  m <- length(input)
  n <- length(input[[1]])
  
    par(mfrow=c(n,m), mar=c(0,0,0,0), oma=c(4,4,4,2))
  for(j in 1:m){
    n <- length(input[[j]])
    for(i in 1:n){
      plot_roc_curves(input[[j]][[i]], cex.axis=1.5, cex.lab=1.5, legend=F, lwd=3, xaxt="n", yaxt="n", ...)
    }
    mtext(names(input)[j],  NORTH<-3, cex=2, line=2) 
  }
}

#freq=25,50,100,200,500
sets <- c(1,2,4)

load("~/flickr/5910219566.Rdat")
deut1 <- lapply(sets,
                function(i) c(sampling[i], indicator_sampling[[i]]))


load("~/flickr/5910198136.Rdat")
drake <- lapply(sets,
                function(i) c(sampling[i], indicator_sampling[[i]]))

load("~/flickr/5906482315.Rdat")
ibm <-  lapply(sets,
                function(i) c(sampling[i], indicator_sampling[[i]]))



load("~/flickr/5909491217.Rdat")
deut3 <- lapply(1:length(freq),
                function(i) c(sampling[i], indicator_sampling[[i]]))
 
load("~/flickr/5909610015.Rdat")
caco3 <-  lapply(1:length(freq),
                function(i) c(sampling[i], indicator_sampling[[i]]))


input <- list(Critical=ibm, Daphnia=drake, GlaciationI=deut1)


source("analysis.R")
png("rocs.png", width=3*4, units="in", height=8, res=400)
roc_effort_plot(input)
dev.off()


###########################
require(socialR)
script <- "analysis_plots.R"
gitaddr <- gitcommit(script)
tags="warningsignals, stochpop"
###########################
upload("rocs.png", script=script, gitaddr=gitaddr, tags=tags)



