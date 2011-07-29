#errorrates.R

roc_fig <- function(null, test, thresh= 5, xlim=NULL, ylim=NULL, bw = "nrd0", 
                    color.null=rgb(0,0,1,.5), color.test=rgb(1,0,0,.5),
                    color.line="black", lwd=3, legend=TRUE, numeric_legend=FALSE,
                    cex.legend=1, add=FALSE, ...){
  nd <- density(null,bw=bw,n=length(null))
  td <- density(test,bw=bw,n=length(test))
## Calculate Axis Limits
  if(is.null(xlim)) xlim <- c(min(td$x, nd$x), max(td$x,nd$x))
  if(is.null(ylim)) ylim <- c(min(td$y, nd$y), max(td$y,nd$y))
  if(!add) 
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
         pch=c(15,15), col=c(color.null, color.test), cex=cex.legend, bty="n")
  if(numeric_legend)
   legend("topright",
         c(paste("False Pos (", 
         prettyNum(false_warning,digits=3), "%)", sep=""), 
         paste("True Pos (", 
         prettyNum(true_warning,digits=3), "%)", sep="")),
         pch=c(15,15), col=c(color.null, color.test), cex=cex.legend,
         bty="n")

  c(false_pos = false_warning/100, true_pos = true_warning/100)
}


