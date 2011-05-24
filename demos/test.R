# figure1.R
require(warningsignals)
require(socialR)
tags="stochpop, warningsignals"
data(ibms)


Rprof("boot.out")
deterior_m<-fit_models(ibm_critical, "LSN")
mc1 <- montecarlotest(deterior_m$const, deterior_m$timedep, cpu=16, nboot=20, times = 20)


#data(drake)
#H6 <- drake_deterior[["H6"]]

