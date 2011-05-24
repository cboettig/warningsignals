# Glaciation data



# Good practice shouldn't require this in a file in data/ directory
require(warningsignals)


# read in the raw data from: 
deut <- read.table("deutnat.txt")

# divisions from: Dakos et. al.
g1 <- which(-deut$V2 > -58800 & -deut$V2 < -12000)
p1 <- which(-deut$V2 > -58800 & -deut$V2 < -17000)
glaciationI   <- data.frame("time"=-deut$V2[p1], "data"=deut$V3[p1])
g2 <- which(-deut$V2 > -151000 & -deut$V2 < -128000)
p2 <- which(-deut$V2 > -151000 & -deut$V2 < -135000)
glaciationII   <- data.frame("time"=-deut$V2[p2], "data"=deut$V3[p2])
g3 <- which(-deut$V2 > -270000 & -deut$V2 < -238000)
p3 <- which(-deut$V2 > -270000 & -deut$V2 < -242000)
glaciationIII   <- data.frame("time"=-deut$V2[p3], "data"=deut$V3[p3])
g4 <- which(-deut$V2 > -385300 & -deut$V2 < -324600)
p4 <- which(-deut$V2 > -385300 & -deut$V2 < -334100)
glaciationIV   <- data.frame("time"=-deut$V2[p4], "data"=deut$V3[p4])

# Data in original time units
glaciation <- list(glaciationI, glaciationII, glaciationIII, glaciationIV)

# Data in time since start, with replicates averaged out
data <- vector("list", 4)
for(i in 1:4){
	X <- glaciation[[i]]
	X <- data.frame("time"=rev(X[,1] - min(X[,1])), "data"=rev(X[,2]))
	require(limma)
	X <-avereps(X, ID=X[,1])
	data[[i]] <- X
}

for(i in 1:4){
	data[[i]] <- dakos_data_processing(data[[i]])
  #plot(data[[i]])
}
#for(i in 1:4) plot_kendalls(data[[i]]$X_ts)


# data we'll use for the examples
deut3 <- data[[3]]$X_ts


