# prosecutorsFallacy_modelfits.R
#' @file prosecutorsFallacy_modelfits.R
#' @brief 
#' @author Carl Boettiger <cboettig@gmail.com>
#' @version 0.0-1
#' @date 2012-02-12
#'
rm(list=ls())
require(reshape2)
require(earlywarning)
load("prosecutor.rda")

## Find out which ones crashed
d <- dim(sn$x1)
crashed <- which(sn$x1[d[1],]==0)


## Let's get tidy data 
dat <- melt(sn$x1)
names(dat) = c("time", "rep", "population")
dat <- cbind(dat, crashed=dat$rep %in% crashed)
dat <- subset(dat, population!=0)

models <- lapply(1:2, function(i)
  stability_model(dat[dat$rep==i, c("time", "population")], "LSN")[1] 
)



# require(plyr) 
#dat <- arrange(dat, crashed) # sort by crashed status

## Need heavy parallelization to handle these estimates over the replicates

library(snow)
## snow method
cluster <- makeCluster(80, type="MPI")
clusterEvalQ(cluster, library(earlywarning)) # load a library
clusterExport(cluster, ls()) # export everything in workspace
models <- parLapply(cluster, 1:1000, function(i)
  stability_model(dat[dat$rep==i, c("time", "population")], "LSN")
)
stopCluster(cluster)
save("models", file="prosecutor_mpi_full.rda")

