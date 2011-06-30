
reformat_tau_dists <- function(taus){
# rename tau_dists so that they follow the same naming convention as likelihood
  lapply(taus, 
    function(pow){
      pow$null_dist <- pow$null_tau_dist[1,] # just tau, not p-values
      pow$test_dist <- pow$test_tau_dist[1,] # just tau, not p-values
      pow})
}

roc_curve <- function(pow, add=FALSE, pts=50, ...){
  n_null <- length(pow$null_dist)
  n_test <- length(pow$test_dist)

  lower <- min(pow$null_dist, pow$test_dist)
  upper <- max(pow$null_dist, pow$test_dist)
  threshold <- seq(lower, upper, length=pts)


  roc <- sapply(threshold, 
                function(thresh){
                  c(sum(pow$null_dist > thresh)/n_null,
	                sum(pow$test_dist > thresh)/n_test)
                })
  roc <- t(roc)

  f<-approx(roc[,1], roc[,2], n=200) 
  delta <- f$x[2] - f$x[1]
  area  <- sum(f$y*delta)
  print(paste("Area Under Curve = ", area))
  if(add)
    lines(roc, ...)
  else
    plot(roc, xlab="False Postive", type="l",
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


