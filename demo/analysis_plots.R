# analysis_plots.R
## Figure 4

rm(list=ls())

require(warningsignals)


roc_effort_plot <- function(input, freq, ...){
# plots as a column
  n <- length(input) # 1..i..n datafiles
  m <- length(input[[1]]) # 1..j..m levels
  
    par(mfrow=c(n,m), mar=c(0,0,0,0), oma=c(4,4,4,2))
  for(j in 1:m){ #row number
    for(i in 1:n){ #work across, col pos
     plot_roc_curves(input[[i]][[j]], cex.axis=1.5, cex.lab=1.5, legend=F, lwd=3, xaxt="n", yaxt="n", ...)
     if(j==1) 
       mtext(names(input)[i],  NORTH<-3, cex=2, line=2) 
     if(i==1)
      mtext(freq[j], WEST<-2, cex=2, line=2)
    }
  }
}

#freq=25,50,100,200,500
sets <- c(1,2,4)

load("~/flickr/5909491217.Rdat")
deut3 <- lapply(1:sets,
                function(i) c(sampling[i], indicator_sampling[[i]]))
load("~/flickr/5909610015.Rdat")
caco3 <-  lapply(sets,
                function(i) c(sampling[i], indicator_sampling[[i]]))
load("~/flickr/5910219566.Rdat")
deut1 <- lapply(sets,
                function(i) c(sampling[i], indicator_sampling[[i]]))
load("~/flickr/5910198136.Rdat")
drake <- lapply(sets,
                function(i) c(sampling[i], indicator_sampling[[i]]))
load("~/flickr/5906482315.Rdat")
ibm <-  lapply(sets,
                function(i) c(sampling[i], indicator_sampling[[i]]))



input <- list(Simulation=ibm, Daphnia=drake, Glaciation=deut3)


source("analysis.R")
png("rocs.png", width=3*4, units="in", height=8, res=400)
roc_effort_plot(input, freq=freq[sets])
dev.off()


###########################
require(socialR)
script <- "analysis_plots.R"
gitaddr <- gitcommit(script)
tags="warningsignals, stochpop"
###########################
upload("rocs.png", script=script, gitaddr=gitaddr, tags=tags)



