# esa_partII.R
T<- 1200
n_pts <- 100
require(warningsignals)

## Collapsing example parameters 
pars = c(Xo = 730, e = 0.5, a = 100, K = 1000, h = 200, 
    i = 0, Da = .09, Dt = 0, p = 2)
## Run the individual based simulation
sn <- saddle_node_ibm(pars, times=seq(0,T, length=n_pts))
## format output as timeseries
ibm_critical <- ts(sn$x1,start=sn$time[1], deltat=sn$time[2]-sn$time[1])


# Stable example parameters
pars = c(Xo = 730, e = 0.5, a = 100, K = 1000, h = 200, 
    i = 0, Da = 0, Dt = 0, p = 2)
## Run the individual based simulation
sn <- saddle_node_ibm(pars, times=seq(0,T, length=n_pts))
# format output as timeseries
ibm_stable  <- ts(sn$x1,start=sn$time[1], deltat=sn$time[2]-sn$time[1])



matrix2ts <- function(X){
    n <- length(X[,1])
    start <- X[1,1]
    end <- X[n,1]
    deltat <- X[2,1]-X[1,1]
    ts(X[,2], start=start, end=end, deltat=deltat)
}
shorten_ts <- function(X, int){
  M <- cbind(time(X)[int], X[int])
  matrix2ts(M)
}

###############################################################################################
int <- 1:90
load("stable_pos.Rdat") # Load a saved ibm_critical for consistency
M <- cbind(time(ibm_stable)[int], ibm_stable[int])
X <- matrix2ts(M)
png("large_tau.png", width=580, height=480)
  tau <- compute_tau(X, "CV")
  mat <-  rbind(c(1),c(2))  # two rows, 1 column
  layout(mat, height = c(.5, 1)) # height of each row
  par(mar=c(0,5,4,2))
  plot(M, xaxt="n", cex.axis=1.5, cex.lab=1.5, lwd=3, col="darkgray",
       type="l", ylab="abundance")
  par(mar=c(4,5,0,2))
  plot_indicator(X, "CV", cor=F, lwd=4, cex.axis=1.5, cex.lab=1.5)
  mtext(bquote(paste("Correlation test, ", tau==.(round(tau[1],3)) )), line=-2.1, cex=2.5)
dev.off()

png("small_tau.png", width=580, height=480)
  load("small_tau.Rdat") # Load a saved ibm_stable for consistency
  M <- cbind(time(ibm_stable)[int], ibm_stable[int])
  Y <- matrix2ts(M)
  tau <- compute_tau(Y, "CV")
  mat <-  rbind(c(1),c(2))  # three rows, 1 column
  layout(mat, height = c(.5, 1)) # height of each row
  par(mar=c(0,5,4,2))
  plot(M, xaxt="n", cex.axis=1.5, cex.lab=1.5, lwd=3, col="darkgray",
       type="l", ylab="abundance")
  par(mar=c(4,5,0,2))
  plot_indicator(Y, "CV", cor=F, lwd=4, cex.axis=1.5, cex.lab=1.5)
  mtext(bquote(paste("Correlation test, ", tau==.(round(tau[1],3)))), line=-2.1, cex=2.5)
dev.off()


int <- 1:85
png("crit_large_tau.png", width=580, height=480)
  load("true_positive.Rdat") # Load a saved ibm_critical for consistency
  M <- cbind(time(ibm_critical)[int], ibm_critical[int])
  X <- matrix2ts(M)

  tau <- compute_tau(X, "CV")
  mat <-  rbind(c(1),c(2))  # three rows, 1 column
  layout(mat, height = c(.5, 1)) # height of each row
  par(mar=c(0,5,4,2))
  plot(M, xaxt="n", cex.axis=1.5, cex.lab=1.5, lwd=3, col="darkgray",
       type="l", ylab="abundance")
  par(mar=c(4,5,0,2))
  plot_indicator(X, "CV", cor=F, lwd=4, cex.axis=1.5, cex.lab=1.5)
  mtext(bquote(paste("Correlation test, ", tau==.(round(tau[1],3)) )), line=-2.1, cex=2.5)
dev.off()

png("crit_small_tau.png", width=580, height=480)
  load("false_negative.Rdat") # Load a saved ibm_critical for consistency
  M <- cbind(time(ibm_critical)[int], ibm_critical[int])
  Y <- matrix2ts(M)
  tau <- compute_tau(Y, "CV")
  mat <-  rbind(c(1),c(2))  # three rows, 1 column
  layout(mat, height = c(.5, 1)) # height of each row
  par(mar=c(0,5,4,2))
  plot(M, xaxt="n", cex.axis=1.5, cex.lab=1.5, lwd=3, col="darkgray",
       type="l", ylab="abundance")
  par(mar=c(4,5,0,2))
  plot_indicator(Y, "CV", cor=F, lwd=4, cex.axis=1.5, cex.lab=1.5)
  mtext(bquote(paste("Correlation test, ", tau==.(round(tau[1],3)))), line=-2.1, cex=2.5)
dev.off()
#######################################




#m <- fit_models(ibm_stable, "LSN")
#taus <- tau_dist_montecarlo(ibm_stable, m$const, m$timedep, nboot=500, cpu=2)
load("esa_taus.Rdat")
nd <- density(taus$null_tau_dist[1,])
td <- density(taus$test_tau_dist[1,])
ylim <- c( min(nd$y, td$y), max(nd$y, td$y))

## PLOT THE NULL DISTRIBUTION OF TAU
png("null.png", width=640, height=640)
par(mar=c(4,5,6,2))
plot(nd, type="n", col=rgb(0,0,1,1), xlim=c(-1,1), ylim=ylim,
        cex.axis=2, cex.lab=2, xlab=expression(paste("Kendall's ", tau)),
        main="")
polygon(nd$x, nd$y, col=rgb(0,0,1,.5), border=rgb(0,0,1))
mtext("Null Distribution", line=2.5, cex=2)
mtext("(stable model, 100 data pts)", line=1, cex=1.5)
dev.off()

## PLOT TEST DISTRIBUTION OF TAU
png("test.png", width=640, height=640)
par(mar=c(4,5,6,2))
plot(nd, type="n", col=rgb(0,0,1,1), xlim=c(-1,1), ylim=ylim,
        cex.axis=2, cex.lab=2, xlab=expression(paste("Kendall's ", tau)),
        main="")
mtext("System approaching transition", line=2.5, cex=2)
mtext("(100 data pts)", line=1, cex=1.5)
polygon(td$x, td$y, col=rgb(1,0,0,.5), border=rgb(1,0,0))
dev.off()

## PLOT BOTH TAU DISTRIBUTIONS
png("both.png", width=640, height=640)
par(mar=c(4,5,6,2))
plot(nd, type="n", col=rgb(0,0,1,1), xlim=c(-1,1), ylim=ylim,
        cex.axis=2, cex.lab=2, xlab=expression(paste("Kendall's ", tau)),
        main="")
polygon(nd$x, nd$y, col=rgb(0,0,1,.5), border=rgb(0,0,1))
mtext("Comparison to a system approaching transition", line=2.5, cex=2)
mtext("(100 data pts)", line=1, cex=1.5)
polygon(td$x, td$y, col=rgb(1,0,0,.5), border=rgb(1,0,0))
dev.off()


png("tau_roc.png", 400, 400)
par(mar=c(4,6,4,2))
auc <- roc_curve(remove_unconverged(reformat_tau_dists(list(taus)))[[1]],
          lwd=5, col="darkblue", cex.axis=2, cex.lab=2)
legend("bottomright", paste("AUC = ", round(auc,2)), cex=2)
dev.off()

############## REPEAT WITH 1000 points #########################
load("esa_taus_1000pts.Rdat") ## STABLE DATA
png("moredata_series.png",580, 480)
  mat <-  rbind(c(1),c(2))  # three rows, 1 column
  layout(mat, height = c(.5, 1)) # height of each row
  par(mar=c(0,5,4,2))
  plot(m$X, xaxt="n", cex.axis=1.5, cex.lab=1.5, lwd=3, col="darkgray",
       type="l", ylab="abundance")
  par(mar=c(4,5,0,2))
  plot_indicator(m$X, "CV", cor=F, lwd=4, cex.axis=1.5, cex.lab=1.5)
  mtext(bquote(paste("Correlation test, ", tau==.(round(tau[1],3)))), line=-2.1, cex=2.5)
dev.off()
#m <- fit_models(ibm_stable, "LSN")
#taus <- tau_dist_montecarlo(ibm_stable, m$const, m$timedep, nboot=500, cpu=2)
nd <- density(taus$null_tau_dist[1,])
td <- density(taus$test_tau_dist[1,])
ylim <- c( min(nd$y, td$y), max(nd$y, td$y))


png("moredata.png")
plot(nd, type="n", col=rgb(0,0,1,1), xlim=c(-1,1), ylim=ylim,
        cex.axis=2, cex.lab=2, xlab=expression(paste("Kendall's ", tau)),
        main="")
polygon(nd$x, nd$y, col=rgb(0,0,1,.5), border=rgb(0,0,1))
polygon(td$x, td$y, col=rgb(1,0,0,.5), border=rgb(1,0,0))
mtext("Comparison", line=2.5, cex=2)
mtext("(1000 data pts)", line=1, cex=1.5)
dev.off()

init.pow <- function(null, test){
  pow$null_dist <- null 
  pow$test_dist <- test 
  pow
}
pow <- init.pow(taus$null_tau_dist[1,], taus$test_tau_dist[1,])

png("roc1000.png", 400, 400)
auc<-roc_curve(pow, lwd=5, col="darkblue", cex.axis=2, cex.lab=2)
legend("bottomright", paste("AUC = ", round(auc,2)), cex=2)
dev.off()
