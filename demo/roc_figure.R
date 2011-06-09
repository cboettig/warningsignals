# Generate a figure to show how ROC curves originate from the distributions
require(warningsignals)
pow <- vector("list", length=2)
class(pow) <- "pow"
dummy <- list(loglik=0, k=0)
class(dummy) <- "gauss"
pow$test <- dummy 
pow$null <- dummy

pow$null_dist <- rnorm(10000, 4, 1)
pow$test_dist <- rnorm(10000, 7, 1)
plot(pow, show_data=F, bw=.05)
