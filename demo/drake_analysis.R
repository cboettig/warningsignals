# analysis.R
require(warningsignals)

###############
require(socialR)
script <- "drake_analysis.R"
gitaddr <- gitcommit(script)
tags="warningsignals, stochpop"
tweet_errors(script, tags=tags)
on.exit(system("git push")) 
##############
source("analysis.R")

data(drake)

m <- fit_models(drake_deterior, "LSN")


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



### Plot methods
## Original plot
png("drake_roc.png"); plot_roc_curves(c(list(mc), taus)); dev.off()
upload("drake_roc.png", script=script, gitaddr=gitaddr, tags=tags)


for(i in 1:length(freq)){
  input <- c(sampling[i], indicator_sampling[[i]])
  file <- paste("drake_", freq[i], ".png", sep="")
  png(file); 
  plot_roc_curves(input, cex.axis=2, cex.lab=2); 
  dev.off()
  upload(file, script=script, gitaddr=gitaddr, tags=tags)

  file <- paste("dist_drake_", freq[i], ".png", sep="")
  png(file, width=480*length(input))
  plot_dists(input, cex.axis=3, cex.lab=3.5); 
  dev.off()
  upload(file, script=script, gitaddr=gitaddr, tags=tags)
}




