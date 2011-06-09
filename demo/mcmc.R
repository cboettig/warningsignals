#mcmc.R
require(mcmcTools)
require(warningsignals)

p <- c(Ro=5.0, m= -.004999, theta=500, sigma=5)
X <- simulateGauss(timedep_LSN, p, N=100, T=100, Xo=500)

# loglik.  note, the name loglik is already defined
f <- function(pars){
  minus_log_lik <- try(lik.gauss(X, pars, timedep_LSN, log=TRUE))
  if(!is(minus_log_lik, "numeric"))
    out <- -Inf
  else
    out <- -minus_log_lik
  out
}

prior <- function(pars){
  2*pars[1]/pars[4]^2
}
pars <- list(p, p)


sfInit(parallel=F, cpu=1)
sfLibrary(mcmcTools)
sfLibrary(warningsignals)
sfExportAll()

chains <- mcmcmc_fn(pars, f, prior, MaxTime=1e4, indep=1e2, 
                    stepsizes=c(0.1, 0.00001, 10, 0.1), Delta_T=2)
burnin=1e2





## plot results
plt <- function(){
  png("mcmc_out.png")
  layout(t(matrix(c(1,1,2,3,4,5), nrow=2)))
  par(cex.lab=1.5, cex.axis=1.5)
  plot(chains[[2]][-burnin, 1], type='l', col=rgb(0,0,1,.8),
       main="mixing chains", ylab="log prob density")
  lines(chains[[1]][-burnin, 1], col="black")
  legend("bottomright", c("normal", "heated"),
         col=c("black", "blue"), lty=1,cex=1.5)
  plot(density((chains[[1]][-burnin, 2])), lwd=3, main="Ro")
  abline(v=5, col="red", lwd=2, lty=2)
  plot(density((chains[[1]][-burnin, 3])), lwd=3, main="m")
  abline(v=-.005, col="red", lwd=2, lty=2)
  plot(density((chains[[1]][-burnin, 4])), lwd=3, main="theta")
  abline(v=500, col="red", lwd=2, lty=2)
  plot(density((chains[[1]][-burnin, 5])), lwd=3, main="sigma")
  abline(v=5, col="red", lwd=2, lty=2)
  dev.off()
}

require(socialR)
social_plot(plt(), file="mcmc_out.png", tags="warningsignals stochpop mcmc",
        width=480, height=3*480)
