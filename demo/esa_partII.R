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

int <- 1:90
X <- cbind(time(ibm_critical)[int], ibm_critical[int])
X <- matrix2ts(X)

png("large_tau.png", width=580, height=480)
tau <- compute_tau(X, "CV")
mat <-  rbind(c(1),c(2))  # three rows, 1 column
layout(mat, height = c(.5, 1)) # height of each row
par(mar=c(0,5,4,2))
plot(X, xaxt="n", cex.axis=1.5, cex.lab=1.5, lwd=3, col="darkgray")
par(mar=c(4,5,0,2))
plot_indicator(X, "CV", cor=F, lwd=4, cex.axis=1.5, cex.lab=1.5)
mtext(bquote(paste("Correlation test, ", tau==.(round(tau[1],3)) )), line=-2.1, cex=1.8)
dev.off()

png("small_tau.png", width=580, height=480)
load("small_tau.Rdat")
Y <- cbind(time(ibm_stable)[int], ibm_stable[int])
Y <- matrix2ts(Y)
tau <- compute_tau(Y, "CV")
mat <-  rbind(c(1),c(2))  # three rows, 1 column
layout(mat, height = c(.5, 1)) # height of each row
par(mar=c(0,5,4,2))
plot(Y, xaxt="n", cex.axis=1.5, cex.lab=1.5, lwd=3, col="darkgray")
par(mar=c(4,5,0,2))
plot_indicator(Y, "CV", cor=F, lwd=4, cex.axis=1.5, cex.lab=1.5)
mtext(bquote(paste("Correlation test, ", tau==.(round(tau[1],3)) )), line=-2.1, cex=1.8)
dev.off()


