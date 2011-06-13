
reformat_tau_dists <- function(taus){
# rename tau_dists so that they follow the same naming convention as likelihood
  lapply(taus, 
    function(pow){
      pow$null_dist <- pow$null_tau_dist
      pow$test_dist <- pow$test_tau_dist
      pow})
}

roc_curve <- function(pow, add=FALSE, ...){
  n_null <- length(pow$null_dist)
  n_test <- length(pow$test_dist)
  false_positive <- seq(0, 1-1/n_null, length=n_null)
  roc <- sapply(false_positive, 
                function(fp){
                  thresh <- sort(pow$null_dist)[
                                 round((1-fp)*n_null)]
	                sum(pow$test_dist > thresh)/n_test})
  delta <-false_positive[2]-false_positive[1]
  area <- sum(roc*delta)
  print(paste("Area Under Curve = ", area))
  if(add)
    lines(false_positive, roc,...)
  else
    plot(false_positive, roc, type="l", xlab="False Postive",
         ylab="True Positive", ylim=c(0,1), xlim=c(0,1), ...)
  curve(1*x, add=TRUE, lty=2)

  area
}


remove_unconverged <- function(pow, nested=FALSE){
## Removes results in which montecarlotest fails to converge
## Indicate failed convergence using both algorithm reporting
## or if the nested model fits better than the richer one...
  null_converged <- which(!as.logical(pow$null_par_dist["convergence",]))
  test_converged <- which(!as.logical(pow$test_par_dist["convergence",]))

  # initialize thes
  null_pos <- 1:length(pow$null_dist)
  test_pos <- null_pos

 if(nested){
    null_pos <- which(pow$null_dist >= 0)
    test_pos <- which(pow$test_dist >= 0)
  }
  pow$null_dist <- pow$null_dist[ c(null_converged, null_pos) ]
  pow$null_par_dist <- pow$null_par_dist[ c(null_converged, null_pos) ]
  pow$test_dist <- pow$test_dist[ c(test_converged, test_pos) ]
  pow$test_par_dist <- pow$test_par_dist[ c(test_converged, test_pos) ]

  pow
}


