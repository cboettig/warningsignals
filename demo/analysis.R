# analysis.R
# custom functions for calling all the analysis and plot functions on a dataset


analysis <- function(data, cpu, nboot, freq){
  m <- fit_models(data, "LSN")
  sampling <- sampling_freq(m$const, m$timedep, cpu=cpu, nboot=nboot,
                            sample_effort=freq, length.original=length(m$X))
  taus <- bootstrap_tau(m$X, m$const, m$timedep, cpu=cpu, nboot=nboot)
  mc <- montecarlotest(m$const, m$timedep, cpu=cpu, nboot=nboot)
  list(m=m, taus=taus, mc=mc, sampling=sampling, data=data, freq=freq)
}

compare_roc_curves <- function(taus, mc, legend=TRUE, ...){
  mc <- remove_unconverged(mc)
  taus <-  reformat_tau_dists(taus) 
  auc1 <- roc_curve(taus[[1]], ...)
  auc2 <- roc_curve(taus[[2]], add=TRUE, col=2, ...)
  auc3 <- roc_curve(taus[[3]], add=TRUE, col=3, ...)
  auc4 <- roc_curve(mc, add=TRUE, col=4, ...)
  if(legend)
    legend("bottomright", c(paste("Var: ", round(auc1,3)), 
                            paste("Acor:", round(auc2, 3)),  
                            paste("Skew:", round(auc3,3)), 
                            paste("Lik: ", round(auc4,3))), 
                            col=c(1,2,3,4), lty=1, cex=1, lwd=2)
}



plot_roc_curves <- function(objects, legend=TRUE, ...){
  # objects a list in which entry contains a fit object with null and 
  # test dists for all the indicators.  

  auc <- numeric(length(objects)) # store area under curve
  legend_txt <- character(length(objects)) # AUC will go in legend

  auc[1] <- roc_curve(objects[[1]], lwd=2, col=1, ...)
  legend_txt[1] <- paste("AUC =",round(auc[1],2))
  for(i in 2:length(objects)){
    auc[i] <- roc_curve(objects[[i]], lwd=2, col=i, add=TRUE, ...)
    legend_txt[i] <- paste("AUC =",round(auc[i],2))
  }
  legend("bottomright",legend_txt, col=c(1:length(objects)), lty=1, lwd=3) 
}






