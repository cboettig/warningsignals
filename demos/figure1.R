# figure1.R
require(warningsignals)
require(socialR)
tags="stochpop, warningsignals"
cpu=16

data(ibms)
social_plot(all_indicators(list(deteriorating=ibm_critical, stable=ibm_stable)), tags=tags)

deterior_m<-fit_models(ibm_critical, "LSN")
deterior_taus <- bootstrap_tau(deterior_m$X, deterior_m$const, 
                               deterior_m$timedep, cpu=cpu, nboot=200)


mc1 <- montecarlotest(deterior_m$const, deterior_m$timedep, cpu=cpu, nboot=200, times = 20)
social_plot(plot(mc1), tag=tags, comment="20 sample pts")
mc2 <- montecarlotest(deterior_m$const, deterior_m$timedep, cpu=cpu, nboot=200, times=50)
social_plot(plot(mc2), tag=tags, comment="50 sample pts")
mc3 <- montecarlotest(deterior_m$const, deterior_m$timedep, cpu=cpu, nboot=200, times=100)
social_plot(plot(mc3), tag=tags, comment="100 sample pts")
mc4 <- montecarlotest(deterior_m$const, deterior_m$timedep, cpu=cpu, nboot=200, times=200)
social_plot(plot(mc4), tag=tags, comment="200 sample pts")
mc5 <- montecarlotest(deterior_m$const, deterior_m$timedep, cpu=cpu, nboot=200, times=400)
social_plot(plot(mc5), tag=tags, comment="400 sample pts")

#data(drake)
#H6 <- drake_deterior[["H6"]]

