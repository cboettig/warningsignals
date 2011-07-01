
sampling_freq <- function(null, test, cpu=16, nboot=200, 
                          sample_effort = c(10, 50, 100),
                          length.original=NULL){
# Evaluate the effect of changing the sampling effort
# Args:
#   m: a set of model fits from fit_models()
#   sample_effort: either a percentage of data or as number of points
#   length.original: if NULL, use abolute lengths, otherwise, sample_effort
#                    is interpreted as a fraction of this original data length
  if(!is.null(length.original))
    sample_effort <- sample_effort * length.original
  lapply(sample_effort, 
         function(effort){
          montecarlotest(null, test, cpu=cpu, nboot=nboot, times=effort)
         })
}


indicator_sampling_freq <- function(m, cpu=16, nboot=200, 
                          sample_effort = c(10, 50, 100),
                          length.original=NULL){
# Evaluate the effect of changing the sampling effort
# Args:
#   m: a set of model fits from fit_models()
#   sample_effort: either a percentage of data or as number of points
#   length.original: if NULL, use abolute lengths, otherwise, sample_effort
#                    is interpreted as a fraction of this original data length
  if(!is.null(length.original))
    sample_effort <- sample_effort * length.original
  out <- lapply(sample_effort, 
         function(effort){
          reformat_tau_dists(bootstrap_tau(m$X, m$const, m$timedep, cpu=cpu, 
                                           nboot=nboot, times=effort))
         })





}

plot_sampling_freq <- function(sampling, freq){
  auc <- numeric(length(sampling)) # area under curve
  auc[1] <- roc_curve(sampling[[1]], lwd=2, col=1)
  legend_txt <- character(length(sampling))
  legend_txt[1] <- paste("Effort", freq[1], "AUC =",round(auc[1],3))
  for(i in 2:length(sampling)){
    sampling[[i]] <- remove_unconverged(sampling[[i]])
    auc[i] <- roc_curve(sampling[[i]], lwd=2, col=i, add=TRUE)
    legend_txt[i] <- paste("Effort", freq[i], "AUC =",round(auc[i],3))
  }
  legend("bottomright",legend_txt, col=c(1:length(sampling)), lty=1, lwd=3) 
}




plot_tau_sampling_freq <- function(sampling_all, freq, pts=pts, stat=1){
  # use stat to indicate which one of the tau summary statistics is plotted
  # usually 1 is variance, 2 is autocorrelation, 3 is skew, 4 is cv
  sampling_vars <- lapply(1:length(sampling_all), 
                          function(i) sampling_all[[i]][[stat]])

  auc <- numeric(length(sampling))
  auc[1] <- roc_curve(sampling[[1]], lwd=2, col=1, pts=pts)
  legend_txt <- character(length(sampling))
  legend_txt[1] <- paste("Effort", freq[1], "AUC =",round(auc[1],3))
  for(i in 2:length(sampling)){
    auc[i] <- roc_curve(sampling[[i]], lwd=2, col=i, add=TRUE, pts=pts)
    legend_txt[i] <- paste("Effort", freq[i], "AUC =",round(auc[i],3))
  }
  legend("bottomright",legend_txt, col=c(1:length(sampling)), lty=1, lwd=3) 
}


