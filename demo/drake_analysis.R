# analysis.R
require(warningsignals)

###############
#require(socialR)
#script <- "drake_analysis.R"
#gitaddr <- gitcommit(script)
#tags="warningsignals, stochpop"
#tweet_errors(script, tags=tags)
#on.exit(system("git push")) 
##############
source("analysis.R")

data(drake)

m <- fit_models(drake_deterior$H6, "LTC")


cpu <- 16
nboot <- 500
freq <- c(25, 50, 100, 200, 500)


## Run the Analyses
sampling <- 
sampling_freq(m$const, m$timedep, cpu=cpu,
              nboot=nboot, sample_effort=freq)

taus <- 
reformat_tau_dists(
  bootstrap_tau(m$X, m$const, m$timedep, 
                cpu=cpu, nboot=nboot))

mc <- 
remove_unconverged(
  montecarlotest(m$const, m$timedep, 
                 cpu=cpu, nboot=nboot)) 

indicator_sampling <- 
indicator_sampling_freq(m, cpu, nboot,
                        sample_effort=freq) 

save(list=ls(), file="drake_ltc.Rdat")

