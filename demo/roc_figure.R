# Generate a figure to show how ROC curves originate from the distributions
rm(list=ls())
require(warningsignals)
null <- rnorm(1000, 5.5, 1)  # Consider a -1, 1 range?
test <- rnorm(1000, 6.5, 1)

init.pow <- function(null, test){
  pow <- vector("list", length=2)
  class(pow) <- "pow"
  dummy <- list(loglik=0, k=0)
  class(dummy) <- "gauss"
  pow$test <- dummy 
  pow$null <- dummy
  pow$null_dist <- null 
  pow$test_dist <- test 
  pow
}
pow <- init.pow(null,test)


## Manuscript Figure 1
png("roc_for_dummies.png", width=3*480, height=2*480)
  par(mfrow=c(2,3), mar=c(7,7,4,2))
  M <- 5  
  t <- seq(3,8,length=M) # sequence of thresholds
  roc_pts <- sapply(1:M, 
                    function(i){ 
                      roc_fig(null, test, thresh=t[i], 
                              xlab="Test Statistic", ylim=c(0,.54), 
                              main="", legend=F, cex=2, cex.axis=3, 
                              cex.lab=3, color.line=i, lwd=5, 
                              numeric_legend=T, cex.legend=2)
                    })
  roc_curve(pow, cex=3, cex.lab=3, cex.axis=2.3, lwd=3)
  points(t(roc_pts), col=1:5, pch=19, cex=4)
  axis(1, col=rgb(0,0,1,.5))
  axis(2, col=rgb(1,0,0,.5))
dev.off()


## ROC film 
M <- 20 # frames
t <- seq(3,8,length=M) # sequence of thresholds
system("rm roc*.png roc.mp4")
roc_pts <- matrix(NA, nrow=2, ncol=M)
for(i in 1:M){
          png(paste("roc_", i, ".png", sep=""),
                    width=600, height=1200)
          par(mfrow=c(2,1), mar=c(4,6,4,2))
          roc_pts[,i] <- roc_fig(null, test, thresh=t[i], 
                  xlab="Test Statistic", ylim=c(0,.54), 
                  main="", legend=F, cex=2, cex.axis=2., 
                  cex.lab=3, color.line="black", lwd=5, 
                  numeric_legend=T, cex.legend=2)
          n_pts <- length(roc_pts[1,])
          col <- gray.colors(n_pts)
          plot(t(roc_pts), pch=19, cex=3, xlim=c(0,1), 
               ylim=c(0,1), ylab="True Positive", col=col,
               xlab="False Positive", cex.lab=3, cex.axis=2.)
          dev.off()
        }
system("ffmpeg -qscale 2 -r 2 -b 9600 -i roc_%d.png roc.mp4")



## Film showing ROC curve as distributions move apart 
tests <- lapply(1:M, function(i) rnorm(1000, i*5/M+5.2, 1))
system("rm pow*.png pow.mp4")
for(i in 1:M){
  png(paste("pow_", i, ".png", sep=""), width=600, height=1200)
  par(mfrow=c(2,1), mar=c(4,6,4,2))
  pow <- init.pow(null,tests[[i]])
  plot(pow, show_text=FALSE, cex.lab=3, cex.axis=2,
  xlab="Test Statistic", xlim=c(2,12))
  roc_curve(pow, cex=2, cex.lab=3, cex.axis=2, lwd=3)
  dev.off()
}
system("ffmpeg -qscale 2 -r 2 -b 9600 -i pow_%d.png pow.mp4")







### Social report
require(socialR)
script <- "roc_figure.R"
gitaddr <- gitcommit(script) # ok to do last since quick-run script
tags="warningsignals, stochpop"

