# prosecutorsFallacy_modelfits.R
#' @file prosecutorsFallacy_modelfits.R
#' @brief 
#' @author Carl Boettiger <cboettig@gmail.com>
#' @version 0.0-1
#' @date 2012-02-12
#'

load("prosecutor.rda")

## Find out which ones crashed
d <- dim(sn$x1)
crashed <- which(sn$x1[d[1],]==0)


## Let's get tidy data 
require(reshape2)
dat <- melt(sn$x1)
names(dat) = c("time", "rep", "population")
dat <- cbind(dat, crashed=dat$rep %in% crashed)
dat <- subset(dat, population!=0)
# require(plyr) 
#dat <- arrange(dat, crashed) # sort by crashed status

## Need heavy parallelization to handle these estimates over the replicates
## see prosecutorsFallacy_modelfits.R

library(snow)
## snow method
cluster <- makeCluster(16, type="MPI")
clusterEvalQ(cluster, library(earlywarning)) # load a library
clusterExport(cluster, ls()) # export everything in workspace
out <- parSapply(cluster, 1:16, function(i)
  stability_model(dat[dat$rep==i, c("time", "population")], "LSN")[1] 
)
print(out)
stopCluster(cluster)

save(list=ls(), file="prosecutor_mpi.rda")

