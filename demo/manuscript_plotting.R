# analysis.R
# custom functions for calling all the analysis and plot functions on a dataset

plot_roc_curves <- function(objects, legend=TRUE, cex.legend=1, hide_auc=FALSE, ...){
  # objects a list in which entry contains a fit object with null and 
  # test dists for all the indicators.  
  stats <- which_statistic(objects)

  auc <- numeric(length(objects)) # store area under curve
  legend_txt <- character(length(objects)) # AUC will go in legend

  auc[1] <- roc_curve(objects[[1]], lty=1, col=1, ...)
  legend_txt[1] <- paste(stats[1], ", ", round(auc[1],2), sep="")
    if(hide_auc)
      legend_txt[1] <- paste(stats[1])
  for(i in 2:length(objects)){
    auc[i] <- roc_curve(objects[[i]], col=i, lty=i, add=TRUE, ...)
    legend_txt[i] <- paste(stats[i], ", ", round(auc[i],2),sep="")
    if(hide_auc)
      legend_txt[i] <- paste(stats[i])
  }
  if(legend)
    legend("bottomright",legend_txt, col=c(1:length(objects)),
           lty=c(1:length(objects)), lwd=3, cex=cex.legend, bty="n") 
}


plot_dists <- function(objects, ...){
  n <- length(objects)
  stats <- which_statistic(objects)
  par(mfrow=c(1,n), mar=c(6,6,4,2))
  for(i in 1:n){
    plot(objects[[i]], xlab=stats[i], ...)
  }
}


which_statistic <- function(objects){
  sapply(objects, function(x){
    if(is(x, "pow")) 
      "Likelihood"
    else if(is(x, "tau_dist_montecarlo"))
        gsub("^(.{8}).*", "\\1",x$signal)  ## TRUNCATE name to 7 chars
  })
}



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
    par(mfrow=c(m,n), mar=c(0,0,0,0), oma=c(5,6,4,2))
    for(j in 1:m){
      for(i in 1:n){
        if(j>1)
          plot(input[[i]][[j]], xlab=stats[j], ylab="", xaxt="n", ylim=c(-.1,1.5),
          yaxt="n", ...)
        else{
          plot(input[[i]][[j]], xlab=stats[j], ylab="", xaxt="n", ylim=c(-.1,0.5),
          yaxt="n", ...)
          mtext(names(input)[i], NORTH<-3, cex=ce)
        }
        if(i==1){
          axis(2, at=c(.1, .5, .9), cex.axis=ce)
          mtext(stats[j], WEST<-2, cex=ce, line=4) 
#          mtext("Density", WEST<-2, cex=ce, line=2) 
        }
        if(j==1 || j==m)
          axis(1, line=-1,cex.axis=ce)
      }
        if(j==m)
          mtext(expression(tau), SOUTH<-1, cex=ce, line=3, outer=TRUE)
    }
}

roc_effort_plot <- function(input, freq, ...){
  n <- length(input) # 1..i..n datafiles
  m <- length(input[[1]]) # 1..j..m levels
  legend=FALSE
    par(mfrow=c(m,n), mar=c(0,0,0,0), oma=c(5,7,5,2))
  for(j in 1:m){ #row number
    for(i in 1:n){ #work across, col pos
     if(i==n && j == m) 
       legend=TRUE ## legend in last plot
     plot_roc_curves(input[[i]][[j]], cex.axis=ce, cex.lab=ce, cex.legend=ce,
                     lwd=2, xaxt="n", yaxt="n", hide_auc=T, legend=legend, ...)
     if(j==m) 
       axis(1,cex.axis=ce) 
     if(j==1) 
       mtext(names(input)[i],  NORTH<-3, cex=ce, line=3) 
     if(i==1){
      mtext(freq[j], WEST<-2, cex=ce, line=5)
      axis(2,cex.axis=ce) 
     }
    }
  }
  mtext("True Positive", outer=TRUE, WEST<-2, cex=1.1*ce, line=3)
  mtext("False Positive", outer=TRUE, SOUTH<-1, cex=1.1*ce, line=3)
}


