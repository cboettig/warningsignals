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


## ROC film 
M <- 10 # frames
t <- seq(4,9,length=M) # sequence of thresholds
system("rm roc*.png roc.mp4")
roc_pts <- matrix(NA, nrow=2, ncol=M)
for(i in 1:M){
        png(paste("roc_", i, ".png", sep=""),
                    width=400, height=800)
        par(mfrow=c(2,1), mar=c(4,6,4,2))

        roc_pts[,i] <- roc_fig(null, test, thresh=t[i], 
                  xlab="Test Statistic", ylim=c(0,.54), 
                  main="", legend=F, cex=2, cex.axis=2., 
                  cex.lab=3, color.line="black", lwd=6, 
                  numeric_legend=T, cex.legend=2)

          ## Show points on the ROC Curve 
          plot(t(roc_pts), pch=19, cex=3, xlim=c(0,1), 
               ylim=c(0,1), ylab="True Positive",
               xlab="False Positive", cex.lab=3, cex.axis=2.)
          abline(v=roc_pts[1,i], col="blue", lwd=4, lty=2)
          abline(h=roc_pts[2,i], col="red", lwd=4, lty=2)
          ## Show Curve
          if(i==M) roc_curve(pow, add=TRUE, lwd=3)
          dev.off()
}

system("ffmpeg -qscale 2 -r 2 -b 9600 -i roc_%d.png roc.mp4")



## Film showing ROC curve as distributions move apart 
tests <- lapply(1:M, function(i) rnorm(1000, i*5/M+5.2, 1))
system("rm pow*.png pow.mp4")
for(i in 1:M){
  png(paste("pow_", i, ".png", sep=""), width=400, height=800)
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

