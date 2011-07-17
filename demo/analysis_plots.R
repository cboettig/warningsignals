# analysis_plots.R

rm(list=ls())

require(warningsignals)

ce <- 1.2

roc_fig3 <- function(input, ...){
  n <- length(input) # 1..i..n datafiles
    par(mfrow=c(1,n))
    for(i in 1:n){ #work across, col pos
     plot_roc_curves(input[[i]], cex.axis=ce, cex.lab=ce, cex.legend=.8,
                     lwd=2, cex.main=ce, legend=TRUE, main=names(input)[i], ...)
    }
}

dists_fig3 <- function(input, ...){
    n <- length(input) # 1..i..n datafiles
    m <- length(input[[1]]) # 1..j..m levels
    stats <- which_statistic(input[[1]])
    par(mfrow=c(m,n), mar=c(0,0,0,0), oma=c(4,4,4,2))
    for(j in 1:m){
      for(i in 1:n){
        plot(input[[i]][[j]], xlab=stats[j], ylab="", ...)
        if(i==1){
          mtext(stats[j], WEST<-2, cex=ce, line=3) 
          mtext("Density", WEST<-2, cex=ce, line=2) 
        }
        if(j==1 || j==m)
          axis(1, line=-1)
      }
    }
}

roc_effort_plot <- function(input, freq, ...){
  n <- length(input) # 1..i..n datafiles
  m <- length(input[[1]]) # 1..j..m levels
  legend=FALSE
    par(mfrow=c(m,n), mar=c(0,0,0,0), oma=c(5,7,4,2))
  for(j in 1:m){ #row number
    for(i in 1:n){ #work across, col pos
     if(i==n && j == m) 
       legend=TRUE ## legend in last plot
     plot_roc_curves(input[[i]][[j]], cex.axis=ce, cex.lab=ce, cex.legend=ce,
                     lwd=2, xaxt="n", yaxt="n", hide_auc=T, legend=legend, ...)
     if(j==m) 
       axis(1,cex.axis=ce) 
     if(j==1) 
       mtext(names(input)[i],  NORTH<-3, cex=ce, line=2) 
     if(i==1){
      mtext(freq[j], WEST<-2, cex=ce, line=5)
      axis(2,cex.axis=ce) 
     }
    }
  }
  mtext("True Positive", outer=TRUE, WEST<-2, cex=1.1*ce, line=3)
  mtext("False Positive", outer=TRUE, SOUTH<-1, cex=1.1*ce, line=3)
}

#freq=25,50,100,200,500
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

appendix <- list("Greenhouse Earth"=caco3, "Glaciation I"=deut1)
appendix_resample <- list("Greenhouse Earth"=caco3_resample, "Glaciation I"=deut1_resample)

roc_data <- list(Simulation=ibm, Daphnia=drake, Glaciation=deut3)
resample <- list(Simulation=ibm_resample, Daphnia=drake_resample, Glaciation=deut3_resample)

source("analysis.R")



png("boettiger_fig3.png", width=8, units="in", height=3, res=400)
roc_fig3(roc_data)
dev.off()

png("boettiger_fig4.png", width=8, units="in", height=8*2/3, res=400)
ylab <- c("25 pts", "50 pts", "200 pts")
roc_effort_plot(resample, freq=ylab)
dev.off()


## Appendix plots
png("appendix3.png", width=8, units="in", height=3*3/2, res=400)
roc_fig3(appendix)
dev.off()
png("appendix4.png", width=8, units="in", height=8*2/3, res=400)
ylab <- c("25 pts", "50 pts", "200 pts")
roc_effort_plot(appendix_resample, freq=ylab)
dev.off()

png("dists_fig4.png", width=8, units="in", height=20, res=400)
dists_fig3(roc_data)
dev.off()

png("dists_a4.png", width=8, units="in", height=4*5, res=400)
dists_fig3(appendix)
dev.off()



###########################
require(socialR)
script <- "analysis_plots.R"
gitaddr <- gitcommit(script)
tags="warningsignals, stochpop"
###########################

#upload("boettiger_fig3.png", script=script, gitaddr=gitaddr, tags=tags, public=0)
#upload("boettiger_fig4.png", script=script, gitaddr=gitaddr, tags=tags, public=0)
#upload("appendix3.png", script=script, gitaddr=gitaddr, tags=tags, public=0)
#upload("appendix4.png", script=script, gitaddr=gitaddr, tags=tags, public=0)

upload("dists_fig4.png", script=script, gitaddr=gitaddr, tags=tags, public=0)
#upload("dists_a4.png", script=script, gitaddr=gitaddr, tags=tags, public=0)



