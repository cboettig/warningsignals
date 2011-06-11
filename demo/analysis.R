require(warningsignals)
data(deuterium)
data(CaCO3) 
data(drake)
data(ibms)

data <- CaCO3
data <- deuterium

all_indicators(data)
m <- fit_models(data, "LSN")
taus <- bootstrap_tau(m$X, m$const, m$timedep, cpu=cpu, nboot=nboot)
mc <- montecarlotest(m$const, m$timedep, cpu=cpu, nboot=nboot)
sampling <- sampling_freq(m)




