# bootstrap_indicators.R

## A few wrapper functions to make it easy to bootstrap a secified set of indicator statistics

fit_models <- function(X, model=c("LTC", "LSN"), integrateOU=FALSE,  
					   optim_method = c("Nelder-Mead", "BFGS", "CG", "L-BFGS-B", "SANN"),
					   ...){
# This function wraps around the updateGauss function which fits the stable and # deteriorating model. It proivdes a mechanism to make intelligent guesses 
# about starting points integrateOU model attempts to calculate the static model
# Args:
#  X: timeseries data
#  model: the form of the alternative model
#  integrateOU: if true, will solve stable model numerically instead of 
#     analytically.  debugging only.  
# optim_method: is passed to the optimizer function, 
#               along with other options in "..."


	optim_method <- match.arg(optim_method)
	lower <- -Inf
	upper <- Inf
	if( optim_method=="L-BFGS-B"){
		lower = c(0, -Inf, -Inf, 0)
		upper = c(Inf, 0, Inf, Inf)
	}

	const_pars <- c(Ro=as.numeric(1/max(time(X))), 
                 theta=as.numeric(mean(X)), sigma=as.numeric(sd(X)))

  ### stable model numerically 
	if (integrateOU){ 
		if (model=="LSN"){
			const <- updateGauss(const_LSN, const_pars, X, method=optim_method, 
								 control=list(maxit=2000), ...)
		} 
		else if (model=="LTC"){
			const <- updateGauss(const_LTC, const_pars, X, method=optim_method, 
								 control=list(maxit=2000), ...)
		}
	}

  ### stable model analytically 
	else {	
	const <- updateGauss(constOU, const_pars, X, method=optim_method, 
						 control=list(maxit=2000), ...)
	}
 if(const$optim_output$convergence!=0){
   print(const$optim_output$message)
    stop("Constant model did not converge, 
          please try different optimizer settings")
  }

  ###  Time dependent model ### 
  ## LTC Model
	if (model=="LTC"){
    	pars <- c(Ro=as.numeric(const$pars["Ro"]), m=0, theta=mean(X), 
    			  sigma=as.numeric(const$pars["sigma"]))
		timedep <- updateGauss(timedep_LTC, pars, X, method=optim_method, 
							   control=list(maxit=2000), upper=upper, lower=lower, ...)
	} 
 

  ## LSN Model
	else if (model=="LSN"){
    # guess LSN parameters from OU parameterization
    guess_Ro <- as.numeric(const$pars['Ro']^2)
    guess_theta <- as.numeric(const$pars['theta']+const$pars['Ro'] )
    guess_sigma<- as.numeric(const$pars['sigma']/sqrt(2*const$pars['Ro']+
                                 const$pars['theta']))
    pars <- c(Ro=guess_Ro, m=0, theta=guess_theta, sigma=guess_sigma)

		timedep <- updateGauss(timedep_LSN, pars, X, method=optim_method, 
							   control=list(maxit=2000), upper=upper, lower=lower, ...)
  }

  ## Check convergence of each model
  const$optim_output$convergence
  if(timedep$optim_output$convergence!=0){
   print(timedep$optim_output$message)
    stop("Time dependent model did not converge, 
          please try different optimizer settings")
  }
  # Outputs in a named list
	list(X=X, const=const, timedep=timedep, pars=pars,
       const_pars=const_pars, model=model)
}


bootstrap_tau <- function(X, const, timedep, 
						  indicators = c("Variance", "Autocorrelation", "Skew", "CV"),
						  nboot=100, cpu=2, windowsize=NULL, 
						  method="kendall", times=NA){
# Tau approach comparison
	taus <- lapply(indicators, 
				   function(stat){
						tau_dist_montecarlo(X, const, timedep, signal=stat, 
						                    nboot=nboot, cpu=cpu, method=method,
                                times=times, windowsize=windowsize) 
						})
	class(taus) <- "bootstrap_tau"
	taus
}


plot.bootstrap_tau <- function(taus, show_p = FALSE, show_error=TRUE, ...){
## If elements of "taus" are of class tau_dist_montecarlo, assumes we have 
## bootstraps for only a single dataset, and plot a single column.   
## If elements are also "bootstrap_taus" then we have a data.frame 
## with bootstraps from multiple datasets, and we plot a matrix with column 
## for each dataset and row for each indicator
	if(is(taus[[1]], "tau_dist_montecarlo")){
	## treat single dataset as data.frame notation as well
		taus <- list(taus)
	}
	data_names <- names(taus)

	## dimensions
	n <- length(taus) ## number of datasets
	m <- length(taus[[1]]) ## number of indicators

	## set up m x n plot-matrix, no margins on subplots, add outer margins
	par(mfrow=c(m,n), oma=c(3,4,2,.2), mar=c(0,0,0,0), ...)

	for(j in 1:m){
		for(i in 1:n){
			if(j == m){ xaxt <- "s"
			} else { xaxt <- "n" }
			if(i > 1){ yaxt <- "n" 
			} else { yaxt <- "s" }

			plot(taus[[i]][[j]], show_p = show_p, show_error=show_error, 
                 xaxt = xaxt, yaxt=yaxt, ...)
			if(j==1) mtext(data_names[i], NORTH<-3, 
                           cex=par()$cex.lab, line=1) ## data labels on top
			if(i==1){
				mtext(taus[[i]][[j]]$signal, WEST<-2, line=3, cex=par()$cex.lab) ## statistic name on first column
				mtext(expression(paste("Prob Density of ", tau)),
						WEST<-2, line=2, cex=.8*par()$cex.lab) ## statistic name 
			}	
			if(j==m & i==2) mtext(expression(paste("Correlation coefficient, ", tau)), SOUTH<-1, line=2, cex=par()$cex.lab) ## x-axis label
		}
	}
}

