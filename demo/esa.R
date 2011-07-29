## Simulate a dataset from the full individual, nonlinear model
T<- 1500
n_pts <- 1000
require(warningsignals)

## Collapsing example parameters 
pars = c(Xo = 550, e = 0.5, a = 200, K = 1000, h = 200, 
    i = 0, Da = .09, Dt = 0, p = 2)
## Run the individual based simulation
sn <- saddle_node_ibm(pars, times=seq(0,T, length=n_pts))
## format output as timeseries
ibm_critical <- ts(sn$x1,start=sn$time[1], deltat=sn$time[2]-sn$time[1])
plot(ibm_critical)



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
m <- fit_models(shorten_ts(ibm_critical,1:900), "LSN")












#load("esa.Rdat")

#load("subtle.Rdat")

ce <- 2.5 # character expansion magnification for plots
M <- 50 # how many points to add each frame?
#windowsize <- 25
system("rm ind_*.png")
for(i in 1:(n_pts/M)){
  int <- 1:(M*i)
  png(paste("ind_", i, ".png", sep=""), height=850, width=1100)

  mat <-  rbind(c(1),c(2),c(3))  # three rows, 1 column
  layout(mat, height = c(1, .5, .5)) # height of each row
  par(mar=c(4,6,4,2))
  plot(time(ibm_critical)[int],ibm_critical[int], lwd=3,
        cex.axis=ce, cex.lab=ce, ylab="Abundance",xlim=c(0,T),
        xlab="Time", type="l", ylim=c(0,800) )


  windowsize <- length(int)/2

  var <- compute_indicator(ibm_critical[int], "CV",
                            windowsize=windowsize)
  window <-  time(ibm_critical)[int]
  par(mar=c(0,6,4,2))
  plot(window[windowsize:length(window)],var, type='l', xlim=c(0,T),
      ylab="CV", xlab="Time", cex.axis=ce, cex.lab=ce, lwd=3, xaxt="n")
  abline(v=window[windowsize], lty=2, col="gray", lwd=4)


  acor <- compute_indicator(ibm_critical[int], "Autocorrelation", 
                            windowsize=windowsize)
  par(mar=c(4,6,0,2))
  plot(window[windowsize:length(window)],acor, type='l', xlim=c(0,T),
      ylab="Autocor", xlab="Time", cex.axis=ce, cex.lab=ce, lwd=3)
  abline(v=window[windowsize], lty=2, col="gray", lwd=4)

  dev.off()
}

# -r is frequency, 2 = 5 frames a second.  
# -qscale is quality, smaller is better.  -b is bitrate. 
system("rm ind.mp4")
system("ffmpeg -qscale 1 -r 2 -b 9600 -i ind_%d.png ind.mp4")
