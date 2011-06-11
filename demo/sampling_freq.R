# figure1.R
require(warningsignals)
require(socialR)
script="sampling_freq.R"
gitcommit(script)
tags="warningsignals"
tweet_errors(script, tags=tags)

cpu=16
nboot <- 10

data(ibms) # from ibm_sims.R

deterior_m<-fit_models(ibm_critical, "LSN")
mc <- sampling_freq(deterior_m, c(20, 50, 100, 200, 400), "total")

# ROC curves for IBM simulated data
# While original data is fixed, method illustrates how increased sampling
# frequency increases the power to detect a shift
mc <- lapply(mc, remove_unconverged)
png("roc_curve.png")
roc_curve(mc[[1]], lwd=2)
roc_curve(mc[[2]], lwd=2, col="blue", add=T)
roc_curve(mc[[3]], lwd=2, col="green", add=T)
roc_curve(mc[[4]], lwd=2, col="red", add=T)
dev.off()
upload(file="roc_curve.png", script=script, tags=tags)


## and repeat with exactly with simulation of stable data.
## This 40 data-pt stable data will probably be estimated with some
## small change in stability (could be of either sign theoretically)
mc <- sampling_freq(deterior_m, c(20, 50, 100, 200, 400), "total")

# ROC curves for IBM simulated data
# While original data is fixed, method illustrates how increased sampling
# frequency increases the power to detect a shift
mc <- lapply(mc, remove_unconverged)
#plot(mc[[1]], shade_aic=T,show_data=F, legend=T)
png("roc_curve.png")
roc_curve(mc[[1]], lwd=2)
roc_curve(mc[[2]], lwd=2, col="blue", add=T)
roc_curve(mc[[3]], lwd=2, col="green", add=T)
roc_curve(mc[[4]], lwd=2, col="red", add=T)
dev.off()
upload(file="roc_curve.png", script=script, tags=tags)


