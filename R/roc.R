
reformat_tau_dists <- function(taus){
# rename tau_dists so that they follow the same naming conv as likelihood
# this lets them work with the same commands as the montecarlotest output
# but does not break any functionality of functions using the old form.
  lapply(taus, 
    function(pow){
      pow$null_dist <- pow$null_tau_dist[1,] # just tau, not p-values
      pow$test_dist <- pow$test_tau_dist[1,] # just tau, not p-values

      # mc plot fns look for this, need to remove. meanwhile: 
      dummy <- list(loglik=0, k=0)
      class(dummy) <- "gauss"
      pow$test <- dummy 
      pow$null <- dummy

      pow})
}

find_threshold <- function(rate, pow,given=c("false.positive",
                                             "true.positive")){
# Returns the threshold value corresponding to a given false postive
#   or true positive rate.  

    given <- match.arg(given)
    threshold <- NULL
    n_null <- length(pow$null_dist)
    n_test <- length(pow$test_dist)
    null_dist <- sort(pow$null_dist) 
    test_dist <- sort(pow$test_dist) 
  
    if(given == "false.positive"){
      # achieve no more than that rate of false positives
      threshold.index <- ceiling((1-rate)*n_null)
      threshold <- null_dist[threshold.index]
    } else {
      # achieve at least that rate of true.positives
      threshold.index <- floor((1-rate)*n_test)
      threshold <- test_dist[threshold.index] 

    }


    f <- function(thresh){
                  c(sum(pow$null_dist > thresh)/n_null,
	                sum(pow$test_dist > thresh)/n_test)
         }

  c(threshold = threshold, false.positive=f(threshold)[1],
    true.positive=f(threshold)[2])
}

roc_curve <- function(pow, add=FALSE, pts=50, ...){
  # Creates the ROC curve from the output of the bootstrap function
  n_null <- length(pow$null_dist)
  n_test <- length(pow$test_dist)

  lower <- min(pow$null_dist, pow$test_dist)
  upper <- max(pow$null_dist, pow$test_dist)
  threshold <- seq(lower, upper, length=pts)

  # Calculate the ROC curve
  roc <- sapply(threshold, 
                function(thresh){
                  c(sum(pow$null_dist > thresh)/n_null,
	                sum(pow$test_dist > thresh)/n_test)
                })
  roc <- t(roc)

  # Caculate the AUC
  f<-approx(roc[,1], roc[,2], n=200) 
  delta <- f$x[2] - f$x[1]
  area  <- sum(f$y*delta)
  print(paste("Area Under Curve = ", area))
  if(add)
    lines(roc, ...)
  else
    plot(roc, xlab="False Positive", type="l",
         ylab="True Positive", ylim=c(0,1), xlim=c(0,1), ...)
  curve(1*x, add=TRUE, lty=2)

  id <- which(roc[,1]<=.05)[1]
  print(paste("True Pos Prob = ", roc[id,2], "at false pos rate of", roc[id,1]))

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


