# Generate a figure to show how ROC curves originate from the distributions
rm(list=ls())
require(warningsignals)

roc_fig <- function(null, test, thresh= 5, xlim=NULL, ylim=NULL, bw = "nrd0", 
                    color.null=rgb(0,0,1,.5), color.test=rgb(1,0,0,.5),
                    color.line="black", lwd=3, legend=TRUE, numeric_legend=FALSE, ...){
  nd <- density(null,bw=bw,n=length(null))
  td <- density(test,bw=bw,n=length(test))
## Calculate Axis Limits
  if(is.null(xlim)) xlim <- c(min(td$x, nd$x), max(td$x,nd$x))
  if(is.null(ylim)) ylim <- c(min(td$y, nd$y), max(td$y,nd$y))
  plot(nd, xlim=xlim, ylim=ylim, type="s", col=color.null, lwd=lwd, ...) 
  lines(td, col=color.test, lwd=lwd)
  shade <- which(nd$x > thresh)
  polygon(c(thresh,nd$x[shade]), c(0,nd$y[shade]),
          col=color.null, border=color.test)
  shade <- which(td$x > thresh)
  polygon(c(thresh,td$x[shade]), c(0,td$y[shade]),
          col=color.test, border=color.test)

  abline(v=thresh, col=color.line, lwd=lwd)

  false_warning <- 100*sum(null > thresh)/length(null)
  true_warning <- 100*sum(test > thresh)/length(test)
  if(legend & !numeric_legend)
    legend("topright",
         c("False Positive","True Positive"),
         pch=c(15,15), col=c(color.null, color.test))
  if(numeric_legend)
   legend("topright",
         c(paste("False Positive (", 
         prettyNum(false_warning,digits=3), "%)", sep=""), 
         paste("True Positive (", 
         prettyNum(true_warning,digits=3), "%)", sep="")),
         pch=c(15,15), col=c(color.null, color.test))

  c(false_pos = false_warning/100, true_pos = true_warning/100)
}

null <- rnorm(1000, 5.5, 1)
test <- rnorm(1000, 6.5, 1)

png("roc_example.png")
par(mar=c(5,5,4,2))
roc_fig(null, test, thresh=5, xlab="Difference in Log Likelihood", main="", numeric_legend=T, cex=2, cex.axis=2, cex.lab=2)
dev.off()


png("roc_for_dummies.png", width=3*480, height=2*480)
par(mfrow=c(2,3), mar=c(5,5,4,2))
t <- seq(3,8,length=5)
roc_pts <- sapply(1:5, function(i) roc_fig(null, test, thresh=t[i], xlab="Difference in Log Likelihood", main="", legend=F, cex=2, cex.axis=2, cex.lab=2, color.line=i, lwd=5))


pow <- vector("list", length=2)
class(pow) <- "pow"
dummy <- list(loglik=0, k=0)
class(dummy) <- "gauss"
pow$test <- dummy 
pow$null <- dummy
pow$null_dist <- null 
pow$test_dist <- test 


roc_curve(pow, cex=2, cex.lab=2, cex.axis=2, lwd=3)
points(t(roc_pts), col=1:5, pch=19, cex=4)

dev.off()

M <- 5
png("ErrorTypes.png", width=480*M)
par(mfrow=c(1,M))
for(t in seq(.5, .95, length=M))
 plot(pow, show_data=F, xlab="Difference in Log Likelihood", shade_aic=T, shade=F, 
      threshold=t, info="threshold", legend=F)
dev.off()

### Social report
require(socialR)
script <- "roc_figure.R"
gitaddr <- gitcommit(script) # ok to do last since quick-run script
tags="warningsignals, stochpop"
upload("ErrorTypes.png", script=script, gitaddr=gitaddr, tags=tags)
upload("roc_example.png", script=script, gitaddr=gitaddr, tags=tags)
upload("roc_for_dummies.png", script=script, gitaddr=gitaddr, tags=tags)

