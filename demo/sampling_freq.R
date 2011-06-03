# figure1.R
require(warningsignals)
require(socialR)
tags="stochpop, warningsignals"
cpu=16
nboot <- 1000

data(ibms)
social_plot(all_indicators(list(deteriorating=ibm_critical, stable=ibm_stable)), tags=tags)

deterior_m<-fit_models(ibm_critical, "LSN")
deterior_taus <- bootstrap_tau(deterior_m$X, deterior_m$const, 
                               deterior_m$timedep, cpu=cpu, nboot=200)


mc1 <- montecarlotest(deterior_m$const, deterior_m$timedep, cpu=cpu, 
                      nboot=nboot, times = 20)
social_plot(plot(mc1), tag=tags, comment="20 sample pts")
mc2 <- montecarlotest(deterior_m$const, deterior_m$timedep, cpu=cpu,
                      nboot=nboot, times=50)
social_plot(plot(mc2), tag=tags, comment="50 sample pts")
mc3 <- montecarlotest(deterior_m$const, deterior_m$timedep, cpu=cpu, 
                      nboot=nboot, times=100)
social_plot(plot(mc3), tag=tags, comment="100 sample pts")
mc4 <- montecarlotest(deterior_m$const, deterior_m$timedep, cpu=cpu, 
                      nboot=nboot, times=200)
social_plot(plot(mc4), tag=tags, comment="200 sample pts")
mc5 <- montecarlotest(deterior_m$const, deterior_m$timedep, cpu=cpu, 
                      nboot=nboot, times=400)
social_plot(plot(mc5), tag=tags, comment="400 sample pts")




# ROC curves for IBM simulated data
# While original data is fixed, method illustrates how increased sampling
# frequency increases the power to detect a shift

mc <- list(mc1, mc2, mc3, mc4)
mc <- lapply(mc, remove_unconverged)
#plot(mc[[1]], shade_aic=T,show_data=F, legend=T)
png("roc_curve.png")
roc_curve(mc[[1]], lwd=2)
roc_curve(mc[[2]], lwd=2, col="blue", add=T)
roc_curve(mc[[3]], lwd=2, col="green", add=T)
roc_curve(mc[[4]], lwd=2, col="red", add=T)
dev.off()
social_report(file="roc_curve.png", tag=c("stochpop", "warningsignals"), 
                comment="")




## and repeat with exactly with simulation of stable data.
## This 40 data-pt stable data will probably be estimated with some
## small change in stability (could be of either sign theoretically)

stable_m<-fit_models(ibm_stable, "LSN")
stable_taus <- bootstrap_tau(stable_m$X, stable_m$const, 
                             stable_m$timedep, cpu=cpu, nboot=200)
mc1s <- montecarlotest(stable_m$const, stable_m$timedep, cpu=cpu, 
                      nboot=nboot, times = 20)
social_plot(plot(mc1s), tag=tags, comment="20 sample pts, stable")
mc2s <- montecarlotest(stable_m$const, stable_m$timedep, cpu=cpu,
                      nboot=nboot, times=50)
social_plot(plot(mc2s), tag=tags, comment="50 sample pts, stable")
mc3s <- montecarlotest(stable_m$const, stable_m$timedep, cpu=cpu, 
                      nboot=nboot, times=100)
social_plot(plot(mc3s), tag=tags, comment="100 sample pts, stable")
mc4s <- montecarlotest(stable_m$const, stable_m$timedep, cpu=cpu, 
                      nboot=nboot, times=200)
social_plot(plot(mc4s), tag=tags, comment="200 sample pts, stable")
mc5s <- montecarlotest(stable_m$const, stable_m$timedep, cpu=cpu, 
                      nboot=nboot, times=400)
social_plot(plot(mc5s), tag=tags, comment="400 sample pts, stable")






#data(drake)
#H6 <- drake_deterior[["H6"]]

