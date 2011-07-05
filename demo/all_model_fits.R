# all_model_fits.R
rm(list=ls())
require(warningsignals)

##########################
#require(socialR)
#script <- "all_model_fits.R"
#gitaddr <- gitcommit(script)
#tags="warningsignals, stochpop"
#tweet_errors(script, tags=tags)
#on.exit(system("git push")) 
#########################

source("analysis.R")

## The analyses -- slow!
data(ibms)
ibm_crit <- fit_models(ibm_critical, "LSN")
ibm_stable <- fit_models(ibm_stable, "LSN")


data(deuterium)
deut1 <- fit_models(deuterium[[1]], "LSN")
deut3 <- fit_models(deuterium[[3]], "LSN")

data(drake)
daphnia <- fit_models(drake_deterior$H6, "LTC")

data(CaCO3) 
caco3 <- fit_models(CaCO3, "LSN")

source("simulate_data.R")
load("linear_sim.rda")

lin_deterior <- fit_models(deteriorating, "LSN")
lin_const <- fit_models(constant, "LSN")


get.mt <- function(fit){
 m <- fit$timedep$pars['m']  
 T <- fit$timedep$T-fit$timedep$t0
 m*T
}

mt <- sapply(list(ibm_crit=ibm_crit, ibm_stable=ibm_stable, deut1=deut1, deut3=deut3, daphnia=daphnia, caco3=caco3, lin_deterior=lin_deterior, lin_const=lin_const), get.mt)

save(list="mt", file="all_model_fits.Rdat")


