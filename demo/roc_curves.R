# ROC Curves for real data examples
# These are now done in plots.R

require(warningsignals)
load("5562383846.Rdat")  #Glaciation, kendall, 2000

reformat_tau_dists <- function(taus){
  lapply(taus, 
    function(pow){
      pow$null_dist <- pow$null_tau_dist
      pow$test_dist <- pow$test_tau_dist
      pow})
}

deut1_taus <- reformat_tau_dists(taus[[1]])
deut2_taus <- reformat_tau_dists(taus[[2]])
deut3_taus <- reformat_tau_dists(taus[[3]])


load("5592395409.Rdat") # glaciation I, II, III mc


png("glaciation_roc.png", width=3*480)
par(mfrow=c(1,3))
roc_curve(deut1_taus[[1]], col=1, main="Glaciation I") # kendall's tau on variance
roc_curve(deut1_taus[[2]], add=TRUE, col=2) # kendall's tau on cor
roc_curve(deut1_taus[[3]], add=TRUE, col=3)
# roc curve for likelihood ratio in deut 1
roc_curve(mc[[1]], add=TRUE, col=4)
legend("bottomright", c("variance", "autocor", "skew", "likelihood"),
       col=c(1,2,3,4), lty=1)


roc_curve(deut2_taus[[1]], col=1, main="Glaciation II") 
roc_curve(deut2_taus[[2]], add=TRUE, col=2) 
roc_curve(deut2_taus[[3]], add=TRUE, col=3)
# roc curve for likelihood ratio in deut 1
roc_curve(mc[[2]], add=TRUE, col=4)
legend("bottomright", c("variance", "autocor", "skew", "likelihood"),
       col=c(1,2,3,4), lty=1)

roc_curve(deut3_taus[[1]], col=1, main="Glaciation III") 
roc_curve(deut3_taus[[2]], add=TRUE, col=2) 
roc_curve(deut3_taus[[3]], add=TRUE, col=3)
# roc curve for likelihood ratio in deut 1
roc_curve(mc[[3]], add=TRUE, col=4)
legend("bottomright", c("variance", "autocor", "skew", "likelihood"),
       col=c(1,2,3,4), lty=1)

dev.off()
require(socialR)
social_report("glaciation_roc.png", tags="warningsignals stochpop roc")


