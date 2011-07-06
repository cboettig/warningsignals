# analysis_plots.R
rm(list=ls())

load("deut1.Rdat")

###########################
require(socialR)
script <- "analysis_plots.R"
gitaddr <- gitcommit(script)
tags="warningsignals, stochpop"
tweet_errors(script, tags=tags)
on.exit(system("git push")) 
###########################

require(warningsignals)
source("analysis.R")

indicator_sampling <- 
indicator_sampling_freq(m, cpu, nboot,
                        sample_effort=freq) 


png("deut1_roc.png"); plot_roc_curves(c(list(mc), taus)); dev.off()
upload("deut1_roc.png", script=script, gitaddr=gitaddr, tags=tags)

for(i in 1:length(freq)){
  input <- c(sampling[i], indicator_sampling[[i]])
  file <- paste("deut1_", freq[i], ".png", sep="")
  png(file); 
  plot_roc_curves(input, cex.axis=2, cex.lab=2); 
  dev.off()
  upload(file, script=script, gitaddr=gitaddr, tags=tags)

#  file <- paste("dist_deut1_", freq[i], ".png", sep="")
#  png(file, width=480*length(input))
#  plot_dists(input, cex.axis=3, cex.lab=3.5); 
#  dev.off()
#  upload(file, script=script, gitaddr=gitaddr, tags=tags)
}



