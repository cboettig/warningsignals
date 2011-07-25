rm(list=ls())
require(warningsignals)
require(socialR)
script <- "ibm_modelchoice.R"
gitaddr <- gitcommit(script)

cpu <- 16
nboot <- 500
## The analyses -- slow!
data(ibms)
lsn <- fit_models(ibm_critical, "LSN")
ltc <- fit_models(ibm_critical, "LTC")
mc <- remove_unconverged(montecarlotest(ltc$timedep, lsn$timedep, 
                                        cpu=cpu, nboot=nboot))

save(list=ls(), file="ibm_modelchoice.Rdat")
