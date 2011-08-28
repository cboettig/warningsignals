# indicators.R
# simple functions to calculate statistics often used as leading indicators of a regime shift

## Dependencies:
## requires psych package for skew & kurosis calculations
## used to require Kendall package, has been replaced with cor.test from the base package, 
## which resolves ties for more accurate p value, but doesn't matter since we focus on tau 
## and because cor.test is used by others i.e. Dakos 2008

#require(psych)

window_var <- function(X, windowsize=length(X)/2){
	sapply(0:(length(X)-windowsize), function(i){
		var(X[(i+1):(i+windowsize)]) 
	})
}

window_cv <- function(X, windowsize=length(X)/2){
	sapply(0:(length(X)-windowsize), function(i){
			var(X[(i+1):(i+windowsize)]) / mean(X[(i+1):(i+windowsize)]) 
		})
}

window_mean <- function(X, windowsize=length(X)/2){
	sapply(0:(length(X)-windowsize), function(i){
		mean(X[(i+1):(i+windowsize)]) 
	})
}

window_skew <- function(X, windowsize=length(X)/2){
	sapply(0:(length(X)-windowsize), function(i){
		skew(X[(i+1):(i+windowsize)]) 
	})
}

window_kurtosi <- function(X, windowsize=length(X)/2){
	sapply(0:(length(X)-windowsize), function(i){
		kurtosi(X[(i+1):(i+windowsize)]) 
	})
}


window_autocorr <- function(X, windowsize=length(X)/2){
	sapply(0:(length(X)-windowsize), 
		function(i){
			a<- acf(X[(i+1):(i+windowsize)], lag.max=1, plot=F) 
			a$acf[2]
		}
	)
}

window_ar.ols <- function(X, windowsize=length(X)/2, demean=FALSE){
	sapply(0:(length(X)-windowsize), 
		function(i){
			a<-ar.ols(X[(i+1):(i+windowsize)], demean=demean)
			a$ar[1]
		}
	)
}


compute_indicator <- function(X, indicator=c("Autocorrelation", "Variance", "Skew", "CV"), windowsize=round(length(X)/2))
## Description Wrapper function to choose warning signal
## Assumes X is a ts object
{
	indicator = match.arg(indicator)
	if(indicator == "Autocorrelation"){
		out <- window_autocorr(X, windowsize)
	} else if(indicator == "Variance"){ 
		out <- window_var(X, windowsize)
	} else if(indicator == "Skew"){
		out <- window_skew(X, windowsize)
	} else if(indicator == "Kurtosis"){
		out <- window_kurtosi(X, windowsize)
	} else if(indicator == "CV"){
		out <- window_cv(X, windowsize)
	} else { warning(paste("Indicator", indicator, "not recognized")) 
	}
	out
}



## Compute and plot the given indicator
plot_indicator <- function(X, indicator=c("Autocorrelation", "Variance",
                           "Skew", "CV"), windowsize=length(X)/2, xpos=0,
                           ypos=90, method=c("kendall", "pearson", "spearman"),
                           pval=TRUE, cor=TRUE, ...)
## Description
## Args:
##		X -- data, either ts object or matrix w/ time in col 1 and data in col 2
##		indicator -- name of the early warning indicator to be computed
##		windowsize -- size of the sliding window over which statistic is calculated
##		xpos, ypos -- % position from bottom left for text to appear
##    cor -- (logical) show the correlation statistic?
##    pval -- (logical) show the p value?
##		... -- extra arguments passed to plot
## Returns:
##		A plot of indicator statistic over time interval, with kendall's tau 
##      and p-val
{	
	method <- match.arg(method)
	if(!is.ts(X)){
		n <- length(X[,1])
		start <- X[1,1]
		end <- X[n,1]
		X <- ts(X[,2], start=start, end=end, freq=(end-start)/n)
	}
	Y <- compute_indicator(X, indicator, windowsize)
	time_window <- time(X)[windowsize:length(X)]
	plot(time_window, Y, xlim=c(start(X)[1], end(X)[1]), type="l",
       xlab="time", ylab=indicator, ...)
	abline(v=time_window[1], lty="dashed")

	out <- cor.test(time(X)[windowsize:length(X)], Y, method=method)

  if(cor){
	w <- c(out$estimate, out$p.value)
    if (method=="kendall"){ 
            text(xshift(0), yshift(ypos),
                 substitute(paste(tau == val), 
                            list(val=round(w[1],2) )),
                 pos=4, cex=par()$cex.axis)
            if(pval){
            text(xshift(0), yshift(ypos-20),
                 substitute(paste("(",p == pval,")"), 
                            list(pval=format.pval(w[2], digits=2))),
                 pos=4, cex=par()$cex.axis)
            }
    }
    else if (method=="pearson") {
            text(xshift(xpos), yshift(ypos),
                 substitute(paste(r == val, "\n (p== ", pval, ")"),
                 list(val=round(w[1],2),pval=format.pval(w[2]))), 
            pos=4, cex=par()$cex.lab)
    }
    else if (method=="spearman") {
            text(xshift(xpos), yshift(ypos),
                 substitute(paste(rho == val, " (p== ", pval, ")"),
                 list(val=round(w[1],2),pval=format.pval(w[2]))), 
            pos=4, cex=par()$cex.lab)
    }
  }
}
	
compute_tau <- function(X, indicator, windowsize=NULL,
                        method=c("kendall", "pearson", "spearman"))
## assumes X is ts object -- should add to a check(?)
{

  method=match.arg(method)
  if(is.null(windowsize))
    windowsize=length(X)/2

	Y <- compute_indicator(X, indicator, windowsize)
	out <- cor.test(time(X)[windowsize:length(X)], Y, method=method)
	c(out$estimate, out$p.value)
}



## PLOTTING FUNCTION
all_indicators <- function(X, indicators = c("Variance", "Autocorrelation", 
                           "Skew", "CV"), method=c("kendall", "pearson",
                           "spearman"), pval=TRUE, cor=TRUE, ...)
## Calc and plot all the leading indicators in a single frame plot
##		using a simple loop over the plot_indicator fn
## Args 
##		X -- can be a list of n ts objects or a single ts object
##		indicators -- a list of indicators m to calculate
##		windowsize -- size of the sliding window over which statistic is calculated
##		xpos, ypos -- % position from bottom left for text to appear
##    cor -- (logical) show the correlation statistic?
##    pval -- (logical) show the p value?
##		... -- extra arguments passed to plot
## Returns:
##      Plot m+1 x n matrix of plots, showing data across the first row
##		 and various indicators below.  Columns for different datasets
{
	if(is(X, "list")){
		n <- length(X) # number of datasets
	} else if (is(X, "ts")){
		n <- 1 # number of datasets
		X <- list(X) 
	} else { 
		warning("class of X not recognized")
	}
    data_names <- names(X)
	m <- length(indicators) # number of indicators

## mar is inner margins, in order bottom, left, top, right. 
## oma is outer margins, default to 0 
	par(mfrow=c(m+1,n), oma=c(4,3,2.5,.2), mar=c(0,1,0,1), ...)
	for(i in 1:n){
		plot(X[[i]], type="l", ylab="Data", xaxt="n", ...)
		mtext(data_names[i],  NORTH<-3, cex=par()$cex.lab, line=1) ## data names on each col
		if(i==1) mtext("Data", WEST<-2, line=3, cex=par()$cex.lab, las=0)  ## "data" y-axis label
	}
## Starts on next row, and goes across datasets
	for(j in 1:m){
		if(j == m){ xaxt <- "s"
		} else {	xaxt <- "n"
		}
		for(i in 1:n){
			plot_indicator(X[[i]], indicators[j], xaxt=xaxt, method=method, xpos=-15, cor=cor, pval=pval, ...) 
			if(i==1) 
        mtext(indicators[j], WEST<-2, line=3, cex=par()$cex.lab, las=0) ## stat name on each row
      ## i == 2 breaks generality of this plot, making the x-axis appear only on the second column always
		}
	}
  mtext("Time", SOUTH<-1, line=2, cex=par()$cex.lab, outer=TRUE) ## x-axis label
}



## a quick labling functions, find percent distance along x and y axis, starting at origin
xshift <- function(xsteps){
	deltax <- (par()$xaxp[2]-par()$xaxp[1])/100
	par()$xaxp[1]+xsteps*deltax
}
yshift <- function(ysteps){
	deltay <- (par()$yaxp[2]-par()$yaxp[1])/100
	par()$yaxp[1]+ysteps*deltay
}





