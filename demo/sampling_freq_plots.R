# ROC curves for IBM simulated data
# While original data is fixed, method illustrates how increased sampling
# frequency increases the power to detect a shift

# shows the 
# plotting script for estimate_required_power.R 
require(socialR)
require(warningsignals)
load("~/flickr/5756092291.Rdat")
mc <- list(mc1, mc2, mc3, mc4)
mc <- lapply(mc, remove_unconverged)

plot(mc[[1]], shade_aic=T,show_data=F, legend=T)


#png("roc_curve.png")
roc_curve(mc[[1]], lwd=2)
roc_curve(mc[[2]], lwd=2, col="blue", add=T)
roc_curve(mc[[3]], lwd=2, col="green", add=T)
roc_curve(mc[[4]], lwd=2, col="red", add=T)
#dev.off()
#social_report(file="roc_curve.png", tag=c("stochpop", "warningsignals"), 
#                comment="uses dataset with flickr-id 5756092291")
