#mcmc.R
require(mcmcTools)
require(warningsignals)

p <- c(Ro=5.0, m= -.004999, theta=500, sigma=5)
X <- simulateGauss(timedep_LSN, p, N=100, T=100, Xo=500)


loglik <- function(pars){
  -lik.gauss(X, pars, timedep_LSN, log=TRUE)
}

prior <- function(pars){
  2*pars[1]/pars[4]^2
}
pars <- list(p, p)

chains <- mcmcmc_fn(pars, loglik, prior, MaxTime=1e5, indep=100, 
                    stepsizes=c(.1, .001, 10, .1), cpu=2)
burnin=1e4


## plot results
png("mcmc_out.png", width=480, height=3*480)

layout(t(matrix(c(1,1,2,3,4,5), nrow=2)))
par(cex.lab=1.5, cex.axis=1.5)
plot(chains[[2]][-burnin, 1], type='l', col=rgb(0,0,1,.8),
     main="mixing chains", ylab="log prob density")
lines(chains[[1]][-burnin, 1], col="black")
legend("bottomright", c("normal", "heated"), col=c("black", "blue"), lty=1,cex=1.5)
plot(density((chains[[1]][-burnin, 2])), lwd=3, main="Ro")
abline(v=5, col="red", lwd=2, lty=2)
plot(density((chains[[1]][-burnin, 3])), lwd=3, main="m")
abline(v=-.005, col="red", lwd=2, lty=2)
plot(density((chains[[1]][-burnin, 2])), lwd=3, main="theta")
abline(v=500, col="red", lwd=2, lty=2)
plot(density((chains[[1]][-burnin, 3])), lwd=3, main="sigma")
abline(v=-5, col="red", lwd=2, lty=2)
dev.off()
flickr("mcmc_out.png", tags="warningsignals stochpop mcmc")
