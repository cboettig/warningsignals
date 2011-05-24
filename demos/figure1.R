# figure1.R
require(warningsignals)

data(ibms)
all_indicators(list(deteriorating=ibm_critical, stable=ibm_stable))

deterior_m<-fit_models(ibm_critical, "LSN")
constant_m<-fit_models(ibm_stable, "LSN")

deterior_taus <- bootstrap_tau(deterior_m$X, deterior_m$const, 
                               deterior_m$timedep)
constant_taus <- bootstrap_tau(constant_m$X, constant_m$const,
                               constant_m$timedep)

#deterior_mc <- montecarlotest(deterior_m$const, deterior_m$timedep, times=10)

constant_mc <- montecarlotest(constant_m$const, constant_m$timedep, times=10)




require(warningsignals)
data(drake)
H6 <- drake_deterior[["H6"]]
all_indicators(list(Daphnia=H6), indicators=c("Var", "Autocor"))
deterior_m<-fit_models(H6, "LSN")
deterior_taus <- bootstrap_tau(deterior_m$X, deterior_m$const, 
                               deterior_m$timedep, cpu=2, nboot=2)
deterior_mc <- montecarlotest(deterior_m$const, deterior_m$timedep, times=10, cpu=2, nboot=2)


