# figure1.R
require(warningsignals)
require(socialR)
tags="stochpop, warningsignals"
data(ibms)


Rprof("test.out")
deterior_m<-fit_models(ibm_critical, "LSN")
mc1 <- montecarlotest(deterior_m$const, deterior_m$timedep, cpu=2, nboot=20, times = 20)


#data(drake)
#H6 <- drake_deterior[["H6"]]

