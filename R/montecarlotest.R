# montecarlotest.R
## Dependencies: snowfall (parallel computing)


## requries loglik and getParameters to be defined functions. Here we just set up there generics.   

# loglik isn't a standard S3 generic, so we add it:
loglik <- function(x, y, ...) UseMethod("loglik")

# getParameters isn't a standard S3 generic, so we add it:
getParameters <- function(x, y, ...) UseMethod("getParameters")


montecarlotest <- function(null, test, nboot = 100, cpu = 2, 
	                         GetParNames=TRUE, times=NA, ...){
# bootstrap the likelihood ratio   
# Args:
#   null: a model with methods "simulate", "update", "loglik", "getParameters"
#   test: another such model
#   nboot: number of replicates
#   cpu: number of cpus available on the machine.  Note: if sfInit has already
#        been called and not stopped, those conditions will be used and this 
#        will be ignored.  
#   GetParNames: optionally attempt to report parameter names in the bootstrap
#   ...: options passed to the optim routine
#   times: simulations can be sampled at specified times.  If NA, will draw
#          sample points in simulating the process that match those of the 
#          original data.  If just a scalar number is given, will use the same 
#          interval, but draw that number of sample points.  If a numeric is 
#          given, will sample at those time intervals.  
#
#          FIXME needs to be extended to sample at times longer/shorter 
#          than original domain.  
# Returns:
#   A list with the following objects:
#       null, test: the models passed in as null and test, 
#       nboot: number of replicate simulations used,
#       null_dist, test_dist: The distribution of Likelihood ratios observed
#                              when simulating under the respective model
#       null_par_dist: distribution of parameters observed for null model, #                      simulating under the null model,
#       test_par_dist: likewise for test model
#       null_sim_test_pars: parameters estimated for the test model over 
#                           simulations for the null model
#       test_sim_null_pars: parameters estimated fro the null model over
#                           simulations for the test model. 
#
	## are we in parallel already? If not, how many cpus do we have?
	if(cpu>1 & !sfIsRunning()){ 	
		sfInit(parallel=TRUE, cpu=cpu) 
		sfLibrary(warningsignals) 
		sfExportAll()
	} else if(cpu<2 & !sfIsRunning()){
    sfInit()
	} 

	## generate each distribution
	null_sim <- sfSapply(1:nboot, function(i){
		data <- simulate(null, times=times)
		if (is(data,"list")) data <- data$rep.1 # handle data that has replicates
		null <- update(null, data, ...)
		test <- update(test, data, ...)
		lr <- -2*(loglik(null) - loglik(test)) 
		list(lr, getParameters(null), getParameters(test))
	})
	test_sim <- sfSapply(1:nboot, function(i){
		data <- simulate(test, times=times)
		if (is(data,"list")) data <- data$rep.1 # handle data that has replicates
		null <- update(null, data, ...)
		test <- update(test, data, ...)
		lr <- -2*(loglik(null) - loglik(test))
		list(lr, getParameters(null), getParameters(test))
	})
	## grab the distribution of likelihood ratios
	null_dist <- unlist(null_sim[1,]) 
	test_dist <- unlist(test_sim[1,]) 
	
	## grab dist of pars observed for null model under null model sims
	null_bootstrap_pars <- matrix( unlist(null_sim[2,]), ncol=nboot)
	if(GetParNames) rownames(null_bootstrap_pars) <- names(getParameters(null))

	## grab dist of pars observed for test model under test model sims
	test_bootstrap_pars <- matrix( unlist(test_sim[3,]), ncol=nboot)
	if(GetParNames) rownames(test_bootstrap_pars) <- names(getParameters(test))

	### Grab the cross-case bootstraps too:
	## grab dist of pars observed for test model under null model sims
	null_sim_test_pars <- matrix( unlist(null_sim[3,]), ncol=nboot)
	if(GetParNames) rownames(null_sim_test_pars) <- names(getParameters(test))

	## grab dist of pars observed for null model under test model sims
	test_sim_null_pars <- matrix( unlist(test_sim[2,]), ncol=nboot)
	if(GetParNames) rownames(test_sim_null_pars) <- names(getParameters(null))

	## format the output
	output <- list(null=null, test=test, nboot=nboot, 
                 null_dist=null_dist, test_dist=test_dist, 
                 null_par_dist = null_bootstrap_pars,
                 test_par_dist = test_bootstrap_pars, 
                 null_sim_test_pars = null_sim_test_pars,
                 test_sim_null_pars = test_sim_null_pars)
	class(output) <- "pow"
	output
}

KL_divergence <- function(null_dist, test_dist){
	nd <- density(null_dist)
	td <- density(test_dist)
	P <- nd$y[nd$y > 0 & td$y > 0]
	Q <- td$y[nd$y > 0 & td$y > 0]
	KL_div <- P %*% log(P/Q)
	KL_div	
}

overlap <- function(pow, bw="nrd0"){
	nd <- density(pow$null_dist, bw=bw)
	td <- density(pow$test_dist, bw=bw)
	td$y %*% nd$y
}

## DOF calculation ##!! Should be made into a generic!!
dof <- function(object){
    dof <- object$k
    if(is.null(object$k)){
      print(paste("cannot determine degrees of freedom, k,
                   please input for model m as m$k"))
  }
  dof
}


## can now specify the info criterion
## plotting function
plot.pow <- function(pow, main="", legend=FALSE, type="density", 
                     test_dist=TRUE, shade_power=FALSE, shade_p=FALSE,
                     show_aic=FALSE, show_data=TRUE, shade=TRUE,
                     shade_aic=FALSE, print_text=TRUE, show_text = c("p"),
                     xlim=NULL, ylim=NULL, null_dist=TRUE, bw = "nrd0",
                     info_criterion=c("aic", "bic", "aicc", "threshold"), ...){

	## Calculate the densities
	nd <- density(pow$null_dist, bw=bw)
	td <- density(pow$test_dist, bw=bw)

	## Calculate Axis Limits
	if(is.null(xlim)) xlim <- c( min(pow$null_dist, pow$test_dist), 
                              max(pow$null_dist, pow$test_dist) )
	if(is.null(ylim)) ylim <- c(min(td$y, nd$y), max(td$y,nd$y))


	## Select the information criterion
	info_criterion = match.arg(info_criterion)
	if(info_criterion=="aic"){
		aic_line <-  2*(dof(pow$test) - dof(pow$null)) 
	} else if(info_criterion=="threshold") {
		threshold_tail <- sort(pow$null_dist)[ round(pow$threshold*pow$nboot) ]
		aic_line <- threshold_tail #nd$x[tmp]
		print(paste("threshold", aic_line))
	}

	## Density plots
	if(type != "hist"){
        if(shade_aic==FALSE){ 
            plottype="n"
        } else { 
            plottype ="s"
        }
		## Plot the null distribution with appropriate shading
		if(null_dist){ 
			plot(nd, xlim=xlim, ylim=ylim, main=main, type=plottype,
           col=rgb(0,0,1,1), ...) 
			if(shade_p){
				shade_p <- which(nd$x > pow$lr)
				polygon(c(pow$lr,nd$x[shade_p]), c(0,nd$y[shade_p]),
                col=rgb(0,0,1,.3), border=rgb(0,0,1,.5))
			} else if(shade){
				polygon(nd$x, nd$y, col=rgb(0,0,1,.3), border=rgb(0,0,1,.5))
			}
		} else { 
			plot(0,0, xlim=xlim, ylim = ylim, main=main, xlab=" Likelihood Ratio",
           ...)  ## blank plot
		}

		## Plot the test distribution with appropriate shading
		if(test_dist){
			if(!null_dist) plot(td, xlim=xlim, main=main, type=plottype,
                          col=rgb(1,0,0,1), ...)  ## just plot test dist 
			else lines(td, type=plottype, col=rgb(1,0,0,1))
			threshold_tail <- sort(pow$null_dist)[ round(pow$threshold*pow$nboot) ]
			if(shade_power){
				shade_power <- which(td$x > threshold_tail)
				polygon(c(threshold_tail, td$x[shade_power]), c(0,td$y[shade_power]), col=rgb(1,0,0,.3), border=rgb(1,0,0,.5))
			} else if(shade){
				polygon(td$x, td$y, col=rgb(1,0,0,.3), border=rgb(1,0,0,.5))
			}
		}
		## AIC shading 
		if(shade_aic){
			shade_p <- which(nd$x > aic_line)
			polygon(c(aic_line,nd$x[shade_p]), c(0,nd$y[shade_p]), 
              col=rgb(1,.5,0,.5), border=rgb(0,0,1,0))
			if(test_dist){ # If test_dist on, shade the errors under the test too
				shade_p <- which(td$x < aic_line)
				polygon(c(aic_line,td$x[shade_p]), c(0,td$y[shade_p]), 
                col=rgb(1,1,0,.5), border=rgb(1,0,0,0))
			}

		}

	## Plot histograms instead of density plots
	} else {
	hist(pow$null_dist, xlim=xlim, lwd=3, col=rgb(0,0,1,.5),
       border="white", main=main, ...)
		if(test_dist){
			hist(pow$test_dist, add=T, lwd=0, col=rgb(1,0,0,.5),
           border="white", main=main, ...)
		}
	}

	## Calculate statistics FIXME (should be a seperate function of pow)
	threshold_tail <- sort(pow$null_dist)[ round(threshold*pow$nboot) ]
	power <- sum(pow$test_dist > threshold_tail)/pow$nboot
	lr <- -2*(loglik(pow$null) - loglik(pow$test))
	p <- 1-sum(pow$null_dist < lr)/pow$nboot
	print(paste("power is ", power, ", p = ", p))
	# Check the reverse test (equivalent to swapping null and test)
	reverse_p <- sum(pow$test_dist < lr)/pow$nboot
	reverse_threshold_tail <- sort(-pow$test_dist)[ round(threshold*pow$nboot) ]
	reverse_power <- sum(-pow$null_dist > reverse_threshold_tail)/pow$nboot
	print(paste("reverse test power ", reverse_power, 
              ", reverse test p = ", reverse_p))
	aic_wrong <- sum(pow$null_dist > aic_line)/pow$nboot
	aic_power <- sum(pow$test_dist > aic_line)/pow$nboot


  ##  Add lines to plots 
	if(show_data){ 
		#abline(v=pow$lr, lwd=3, col="darkred", lty=2 )
		points(lr,yshift(1), cex=1.5, col="black", pch=25, fg="black", 
            bg="black")
	}
	if(show_aic){
    #    abline(v=aic_line, lwd=3, col="darkgray", lty=3) 
    	points(aic_line,yshift(1), cex=1.5, col="red", pch=25, fg="red", bg="red")
}


	## add legend
	if(legend){
        if (shade==TRUE){
		    legend("topright", c("sim under Null", "sim under Test", "observed"), 
               pch=c(15,15,25), fg=c("white", "white", "black"), 
               col=c(rgb(0,0,1,.5), rgb(1,0,0,.5), "black"))
        }
        else if (shade_aic==TRUE & test_dist==TRUE){
   		    legend("topright", 
                 c( paste("False Alarms (", round(aic_wrong*100,3),
                          "%)", sep=""), 
                    paste("Missed Events (", round((1-aic_power)*100,3),
                          "%)", sep="")),
                    pch=c(15,15), col=c(rgb(1,.5,0,.5), rgb(1,1,0,.5)))
        }
        else if (shade_aic==TRUE & test_dist==FALSE){
   		    legend("topright", paste("False Alarms (", round(aic_wrong*100,3),
                 "%)", sep=""), pch=15, col=rgb(1,.5,0,.5))
        }
    }
}

