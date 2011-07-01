# Generate a figure to show how ROC curves originate from the distributions
rm(list=ls())
require(warningsignals)
null <- rnorm(1000, 5.5, 1)
test <- rnorm(1000, 6.5, 1)

png("roc_example.png")
par(mar=c(5,5,4,2))
  roc_fig(null, test, thresh=5, xlab="Test Statistic", main="", numeric_legend=T, cex.axis=2, cex.lab=2, cex.legend=1.5, ylim=c(0,.54))
dev.off()


png("roc_for_dummies.png", width=3*480, height=2*480)
par(mfrow=c(2,3), mar=c(7,7,4,2))
t <- seq(3,8,length=5)
roc_pts <- sapply(1:5, 
                  function(i) 
                    roc_fig(null, test, thresh=t[i], 
                    xlab="Test Statistic", ylim=c(0,.54), 
                    main="", legend=F, cex=2, cex.axis=3, cex.lab=3, 
                    color.line=i, lwd=5, numeric_legend=T, cex.legend=3))

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

roc_curve(pow, cex=3, cex.lab=3, cex.axis=2.3, lwd=3)
points(t(roc_pts), col=1:5, pch=19, cex=4)
axis(1, col=rgb(0,0,1,.5))
axis(2, col=rgb(1,0,0,.5))

dev.off()



png("compare_rocs.png", width=3*480, height=2*480)
par(mfrow=c(2,3), mar=c(5,5,4,2))
t <- seq(3,8,length=5)
tests <- lapply(1:3, function(i) rnorm(1000, i+4.5, 1))
for(i in 1:3){
  pow <- init.pow(null,tests[[i]])
  plot(pow, show_text=FALSE, cex.lab=3, cex.axis=2.3, xlab="Test Statistic")
}

for(i in 1:3){
  test <- rnorm(1000, i+4.5, 1)
  pow <- init.pow(null,tests[[i]])
  roc_curve(pow, cex=2, cex.lab=3, cex.axis=2.3, lwd=3)
}
dev.off()






M <- 5
png("ErrorTypes.png", width=480*M)
par(mfrow=c(1,M))
for(t in seq(.5, .95, length=M))
 plot(pow, show_data=F, xlab="Test Statistic", shade_aic=T, shade=F, 
      threshold=t, info="threshold", legend=F, cex=2, cex.axis=2, cex.lab=2)
dev.off()





### Social report
require(socialR)
script <- "roc_figure.R"
gitaddr <- gitcommit(script) # ok to do last since quick-run script
tags="warningsignals, stochpop"
upload("compare_rocs.png", script=script, gitaddr=gitaddr, tags=tags)
upload("roc_example.png", script=script, gitaddr=gitaddr, tags=tags)
upload("roc_for_dummies.png", script=script, gitaddr=gitaddr, tags=tags)

