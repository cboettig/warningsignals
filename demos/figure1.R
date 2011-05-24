# figure1.R
require(warningsignals)

#data(ibms)
#all_indicators(list(deteriorating=ibm_critical, stable=ibm_stable))

data(drake)
H6 <- drake_deterior[["H6"]]
all_indicators(list(Daphnia=H6), indicators=c("Var", "Autocor"))
deterior_m<-fit_models(H6, "LSN")
deterior_taus <- bootstrap_tau(deterior_m$X, deterior_m$const, 
                               deterior_m$timedep, cpu=2, nboot=2)

mc1 <- montecarlotest(deterior_m$const, deterior_m$timedep, cpu=16, nboot=200)
mc2 <- montecarlotest(deterior_m$const, deterior_m$timedep, cpu=16, nboot=200, times=50)
mc3 <- montecarlotest(deterior_m$const, deterior_m$timedep, cpu=16, nboot=200, times=100)
mc4 <- montecarlotest(deterior_m$const, deterior_m$timedep, cpu=16, nboot=200, times=200)

require(socialR)
social_plot(plot(mc1))
social_plot(plot(mc2))
social_plot(plot(mc3))
social_plot(plot(mc4))

