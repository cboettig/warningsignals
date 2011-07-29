## Simulate a dataset from the full individual, nonlinear model
T<- 2200
n_pts <- 100
cpu <- 16
require(warningsignals)
require(socialR)
"esa_partIII.R"
gitaddr <- gitcommit(script)
tags="warningsignals, stochpop"

matrix2ts <- function(X){
    n <- length(X[,1])
    start <- X[1,1]
    end <- X[n,1]
    deltat <- X[2,1]-X[1,1]
    ts(X[,2], start=start, end=end, deltat=deltat)
}

## Collapsing example parameters 
pars = c(Xo = 730, e = 0.5, a = 100, K = 1000, h = 200, 
    i = 0, Da = .04, Dt = 0, p = 2)
## Run the individual based simulation
sn <- saddle_node_ibm(pars, times=seq(0,T, length=n_pts))
## format output as timeseries
ibm_critical <- ts(sn$x1,start=sn$time[1], deltat=sn$time[2]-sn$time[1])


ce <- 2.5 # character expansion magnification for plots
M <- 10 # how many points to add each frame?
for(i in 1:(n_pts/M)){
  int <- 1:(M*i)
  png(paste("uncertainty_acor_", i, ".png", sep=""), height=850, width=1100)

  # Plot size/layout
  mat <-  rbind(c(1),c(2),c(3))  # three rows, 1 column
  layout(mat, height = c(1, .5, .5)) # height of each row
  par(mar=c(4,6,4,2))

  #### Create the time series plot #########
  plot(time(ibm_critical)[int],ibm_critical[int], lwd=3,
        cex.axis=ce, cex.lab=ce, ylab="Abundance",xlim=c(0,T*1.5),
        xlab="Time", type="l", ylim=c(0,800) )

  ###### Calc and plot the Coeff. of Variance indicator ########
  windowsize <- length(int)/2 # sliding window size to compute indicator
  var <- compute_indicator(ibm_critical[int], "Autocorrelation",
                            windowsize=windowsize)
  window <-  time(ibm_critical)[int] # get the time interval used
  par(mar=c(0,6,4,2)) # this plot goes flush against the next
  plot(window[windowsize:length(window)],var, type='l', xlim=c(0,T*1.5),
      ylab="Autocorr", xlab="Time", cex.axis=ce, cex.lab=ce, lwd=3, xaxt="n")
  abline(v=window[windowsize], lty=2, col="gray", lwd=4) #mark interval start

  #### Compute & plot the ROC curve ############
  Y <- cbind(time(ibm_critical)[int], ibm_critical[int])
  X <- matrix2ts(Y)
  m <- fit_models(X, "LTC")
  taus <- tau_dist_montecarlo(X, m$const, m$timedep, nboot=500, cpu=cpu,
                              signal="Autocorrelation")
  roc_curve(remove_unconverged(reformat_tau_dists(list(taus)))[[1]],
          lwd=4, col="darkblue", cex.axis=2, cex.lab=2)
  dev.off()
}


#save(list=ls(), file="esaIII.Rdat")

## -r is frequency, 2 = 5 frames a second.  
## -qscale is quality, smaller is better.  -b is bitrate. 
system("rm uncertainty.mp4")
system("ffmpeg -qscale 5 -r 4 -b 9600 -i uncertainty_%d.png uncertainty.mp4")

#upload("uncertainty_1.png", script=script, gitaddr=gitaddr, tags=tags, public=0)

