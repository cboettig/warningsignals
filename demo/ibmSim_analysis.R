require(warningsignals)
data(ibms)



m <- fit_models(ibm_critical)
taus <- reformat_tau_dists(bootstrap_tau(m$X, m$const, m$timedep, cpu=16, nboot=32))
mc <- remove_unconverged(montecarlotest(m$const, m$timedep, cpu=16, nboot=32))



#plot(ibm_critical) 
#plot(mc, "likelihood ratio distributions for simulated data")

#require(ggplot2)



