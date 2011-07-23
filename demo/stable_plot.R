require(warningsignals)
load("ibm_stable3.Rdat")

require(socialR)
script <- "stable_plot.R"
gitaddr <- gitcommit(script)
tags="warningsignals, stochpop"


tweet_errors(script, tags=tags)
### Plot methods
## Original plot
png("ibm_stable_roc.png"); plot_roc_curves(c(list(mc), taus)); dev.off()
upload("ibm_stable_roc.png", script=script, gitaddr=gitaddr, tags=tags)


for(i in 1:length(freq)){
  input <- c(sampling[i], indicator_sampling[[i]])
  file <- paste("ibm_stable_", freq[i], ".png", sep="")
  png(file); 
  plot_roc_curves(input, cex.axis=2, cex.lab=2); 
  dev.off()
  upload(file, script=script, gitaddr=gitaddr, tags=tags)

  file <- paste("dist_ibm_stable_", freq[i], ".png", sep="")
  png(file, width=480*length(input))
  plot_dists(input, cex.axis=3, cex.lab=3.5); 
  dev.off()
 # upload(file, script=script, gitaddr=gitaddr, tags=tags)
}




