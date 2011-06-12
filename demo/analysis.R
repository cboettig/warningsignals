# analysis.R
require(warningsignals)
require(socialR)
script <- "analysis.R"
gitcommit(script)
tags="warningsignals"
tweet_errors(script, tags=tags)
on.exit(system("git push")) 

cpu <- 16
nboot <- 2000
freq <- c(.1, .5, 1.5, 2, 5, 10)

analysis <- function(data){
  m <- fit_models(data, "LSN")
  taus <- bootstrap_tau(m$X, m$const, m$timedep, cpu=cpu, nboot=nboot)
  mc <- montecarlotest(m$const, m$timedep, cpu=cpu, nboot=nboot)
  sampling <- sampling_freq(m, freq)
  list(m=m, taus=taus, mc=mc, sampling=sampling)
}

data(CaCO3) 
caco3 <- analysis(CaCO3)
save(list="caco3", file="caco3_analysis.rda")

data(deuterium)
deut <- lapply(deuterium, analysis)
save(list="deut", file="deut_analysis.rda")

data(ibms)
ibm_crit <- analysis(ibm_critical)
ibm_stab <- analysis(ibm_stable)
save(list=c("ibm_crit", "ibm_stab"), file="ibms_analysis.rda")


data(drake)
drake <- analysis(drake_deterior$H6)
save(list="drake", file="drake_analysis.rda")




