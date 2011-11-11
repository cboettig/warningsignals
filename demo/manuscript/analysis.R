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


