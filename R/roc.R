
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

  if(add)
    lines(false_positive, roc,...)
  else
    plot(false_positive, roc, type="l", xlab="False Postive",
         ylab="True Positive", ylim=c(0,1), xlim=c(0,1), ...)
  curve(1*x, add=TRUE, lty=2)
}


remove_unconverged <- function(pow){
  null_unconverged <- which(as.logical(pow$null_par_dist["convergence",]))
  null_negative <- which(pow$null_dist < 0)
  new_null_dist <- pow$null_dist[ -c(null_unconverged, null_negative) ]
  new_null_par_dist <- pow$null_par_dist[-c(null_unconverged, null_negative) ] 

  test_unconverged <- which(as.logical(pow$test_par_dist["convergence",]))
  test_negative <- which(pow$test_dist < 0)
  new_test_dist <- pow$test_dist[ -c(test_unconverged, test_negative) ]
  new_test_par_dist <- pow$test_par_dist[-c(test_unconverged, test_negative) ] 

  out <- pow
  out$null_dist <- new_null_dist
  out$null_par_dist <- new_null_par_dist
  out$test_dist <- new_test_dist
  out$test_par_dist <- new_test_par_dist
  out
 }



