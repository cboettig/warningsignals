#ibm_sampling_tau.R
load("ibm_sampling_tau.Rdat")
require(warningsignals)

############
require(socialR)
script <- "ibm_sampling_tau.R"
gitaddr <- gitcommit(script)
tags="warningsignals, stochpop"
tweet_errors(script, tags=tags)
on.exit(system("git push")) 
############

source("analysis.R")
source("../R/sampling_freq.R")


stat <- 3 # 1 is var, 2 autcor, 3 skew, 4 CV

png("tau_sampling.png"); 
plot_tau_sampling_freq(sampling, freq, pts=100, stat=stat); 
dev.off()
upload("tau_sampling.png", script=script, gitaddr=gitaddr, tags=tags)



png("dists.png", width=2*480, height=2*480)
par(mfrow=c(2,2))
for(i in 1:length(sampling))
  plot(sampling[[i]][[stat]], xlab=paste("tau for trend in stat", stat)
       cex=2, cex.axis=2, cex.lab=2)
dev.off()
upload("dists.png", script=script, gitaddr=gitaddr, tags=tags)




# Generate a figure to show how ROC curves originate from the distributions
null <- sampling[[4]][[stat]]$null_tau_dist[1,]
test <- sampling[[4]][[stat]]$test_tau_dist[1,]

png("roc_example.png")
par(mar=c(5,5,4,2))
  roc_fig(null, test, thresh=0, xlab="Test Statistic", main="", numeric_legend=T, cex.axis=2, cex.lab=2, cex.legend=1.5)
dev.off()
upload("roc_example.png", script=script, gitaddr=gitaddr, tags=tags)



