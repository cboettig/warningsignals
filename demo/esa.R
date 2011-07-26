## Simulate a dataset from the full individual, nonlinear model
T<- 1200
n_pts <- 1000
require(warningsignals)

## Collapsing example parameters 
pars = c(Xo = 730, e = 0.5, a = 100, K = 1000, h = 200, 
    i = 0, Da = .09, Dt = 0, p = 2)

## Run the individual based simulation
sn <- saddle_node_ibm(pars, times=seq(0,T, length=n_pts))
## format output as timeseries
ibm_critical <- ts(sn$x1,start=sn$time[1], deltat=sn$time[2]-sn$time[1])


#load("esa.Rdat")

ce <- 2.5 # character expansion magnification for plots
M <- 50
windowsize <- 25
for(i in 1:(n_pts/M)){
  int <- 1:(M*i)
  png(paste(i, ".png", sep=""), width=11, height=8.5,
      units="in", res=300)

  mat <-  rbind(c(1),c(2),c(3))  # three rows, 1 column
  layout(mat, height = c(1, .5, .5)) # height of each row
  par(mar=c(4,6,4,2))
  plot(time(ibm_critical)[int],ibm_critical[int], lwd=3,
        cex.axis=ce, cex.lab=ce, ylab="Abundance",xlim=c(0,T*1.5),
        xlab="Time", type="l", ylim=c(0,800) )


#  windowsize <- length(int)/2

  var <- compute_indicator(ibm_critical[int], "CV",
                            windowsize=windowsize)
  window <-  time(ibm_critical)[int]
  par(mar=c(0,6,4,2))
  plot(window[windowsize:length(window)],var, type='l', xlim=c(0,T*1.5),
      ylab="CV", xlab="Time", cex.axis=ce, cex.lab=ce, lwd=3, xaxt="n")


  acor <- compute_indicator(ibm_critical[int], "Autocorrelation", 
                            windowsize=windowsize)
  par(mar=c(4,6,0,2))
  plot(window[windowsize:length(window)],acor, type='l', xlim=c(0,T*1.5),
      ylab="Autocor", xlab="Time", cex.axis=ce, cex.lab=ce, lwd=3)

  dev.off()
}

# -r is frequency, 2 = 5 frames a second
# -qscale is quality, smaller is better.  -b is bitrate. 
system("rm movie.mp4")
system("ffmpeg -qscale 5 -r 2 -b 9600 -i %d.png movie.mp4")
system("rm *.png")
