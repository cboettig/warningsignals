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

ce<-1.5
lwd<-3
## ROC film 
M <- 4 # frames
t <- seq(5,8,length=M) # sequence of thresholds
roc_pts <- matrix(NA, nrow=2, ncol=M)
cairo_pdf(paste("Fig1", ".pdf", sep=""), 
                    width=8, height=8*2/M)
mat <-  cbind(c(1,2),c(3,4),c(5,6),c(7,8))
layout(mat, height = c(1, 1)) # height of each row
          par(oma=c(5,5,5,0))
for(i in 1:M){
          col <- rev(palette(gray(seq(0,.7,len=i+1))))
          par(mar=c(1,0,0,0))
        nd <- density(null,n=length(null))
        plot(nd, xlim=c(2,11), type="s", main="",
             cex.axis=ce, cex.lab=ce, xlab="",
             lwd=lwd, col=rgb(0,0,1,.5), xaxt="n", yaxt="n") 

        ## Show history of lines
        for(j in 1:i) 
          abline(v=t[j],lwd=lwd, col=col[j], lty=2)

        roc_pts[,i] <- roc_fig(null, test, thresh=t[i], 
                  legend=F, cex=ce, 
                  color.line="black", lwd=lwd, 
                  numeric_legend=F, cex.legend=1, add=T)

        if(i==1) 
          axis(2, cex.axis=ce, yaxp=c(.1,.4,4))
        axis(3, cex.axis=ce, xaxp=c(2,10,2))

          ## Show points on the ROC Curve 
          plot(t(roc_pts), pch=19, cex=ce*1.5, xlim=c(0,1), 
               ylim=c(0,1), ylab="", col=col, yaxt="n",
               xlab="", cex.axis=ce)
          abline(v=roc_pts[1,i], col="blue", lwd=lwd*.8, lty=1)
          abline(h=roc_pts[2,i], col="red", lwd=lwd*.8, lty=1)
          points(t(roc_pts), pch=19, cex=ce*1.5, col=col)
          if(i==1){
           axis(2,cex.axis=ce, yaxp=c(0.,1.,4))
           points(t(roc_pts), cex=ce*1.5, col="black", pch=19)
          }
       
          ## Show Curve
          if(i==M) roc_curve(pow, add=TRUE, lwd=lwd)
}
mtext("Test statistic (arbitrary units)", side=3, outer=TRUE,cex=1,line=3)
mtext("False Positive", side=1, outer=TRUE,cex=1,line=3)
mtext("        True Positive", side=2, outer=TRUE,cex=1, adj=0, line=3)
mtext("Density          ", side=2, outer=TRUE,cex=1, adj=1, line=3)
dev.off()


