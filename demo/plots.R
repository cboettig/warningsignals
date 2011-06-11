## results.R
## Author: Carl Boettiger 
## Load the data files for all stored results


require(warningsignals)

## Kendall's Tau distributions  2000 reps: (data for Fig 1b)
## (confirm these are all tau and not rho)?
##  sim LSN (deterior and constant), deut3 and CaCO3 data:
load("../data/5554848679.Rdat") 
## Glaciation I, II, III (tau):
load("../data/5562383846.Rdat")  

## LR distributions, 2000 reps, Fig 1c: 
## deterior (LSN sim), constant (OU sim), deut3, CaCO3 data
load("../data/35563325713.Rdat")
##  GlaciationI, II, III (mc): 
load("../data/5592395409.Rdat")
## Drake Data example, for all figure types
load("../data/reanalyze.Rdat"); drake <- out


## Original Figure 1b plots
## use just indicators 1 and 2: as in deterior_taus[1:2] (var and autocorr)
jpeg(file="Boettiger_fig2.jpg", height=2*37, width=183, units="mm", quality=100, res=150)
plot.bootstrap_tau(list(Constant=constant_taus[1:2],
                   Deteriorating=deterior_taus[1:2], 
                   Glaciation=taus[[3]][1:2], 
                   Daphnia=drake$taus[["H6"]][1:2]),
           cex.axis=.8, cex.lab=.8, show_p=FALSE, ylim=c(0,2.8), yaxp = c(0, 3, 3), xaxp=c(-1,1,5))
dev.off()


## Replace with corresponding ROC curves

## Constant Env Sim
compare_roc_curves <- function(taus, mc, legend=TRUE, ...){
  taus <-  reformat_tau_dists(taus) 
  auc1 <- roc_curve(taus[[1]], ...)
  auc2 <- roc_curve(taus[[2]], add=TRUE, col=2, ...)
  auc3 <- roc_curve(taus[[3]], add=TRUE, col=3, ...)
  auc4 <- roc_curve(mc, add=TRUE, col=4, ...)
  if(legend)
    legend("bottomright", c(paste("Var: ", round(auc1,3)), 
                            paste("Acor:", round(auc2, 3)),  
                            paste("Skew:", round(auc3,3)), 
                            paste("Lik: ", round(auc4,3))), 
                            col=c(1,2,3,4), lty=1, cex=3, lwd=3)
}


#cairo_pdf(file="boettigerfig2b.pdf", width=7*4, height=7)
png(file="boettigerfig2b.png", width=480*4, height=480*2)
par(mfrow=c(2,4))
## Constant Env Sim
compare_roc_curves(constant_taus, constant_mc, main="Constant", cex=3, cex.lab=3, cex.axis=3, lwd=3,cex.main=2.5)
## Deteriorating Env Sim
compare_roc_curves(deterior_taus, deterior_mc, main="Deteriorating", cex=3, cex.lab=3, cex.axis=3, lwd=3,cex.main=2.5)
## Glaciation III Data
compare_roc_curves(taus[[3]], mc[[3]], main="Glaciation", cex=3, cex.lab=2, cex.axis=3, lwd=3,cex.main=2.5)  
## Daphnia, consider non-H6
compare_roc_curves(drake$taus[["H6"]], drake$mc[["H6"]], main="Daphnia", legend=T, cex=3, cex.lab=3, cex.axis=3, lwd=3,cex.main=2.5)  
#dev.off()


# ibm data
load("../data/5615442069.Rdat") # mc data
load("../data/5613811639.Rdat") # tau data

#par(mfrow=c(1,4))
## Constant Env Sim
compare_roc_curves(constant_taus, constant_mc, main="Constant", cex=3, cex.lab=3, cex.axis=3, lwd=3,cex.main=2.5)
## Deteriorating Env Sim
compare_roc_curves(deterior_taus, deterior_mc, main="Deteriorating", cex=3, cex.lab=3, cex.axis=3, lwd=3,cex.main=2.5)
## Glaciation I & II Data
compare_roc_curves(taus[[1]], mc[[1]], main="Glaciation I", cex=3, cex.lab=3, cex.axis=3, lwd=3,cex.main=2.5)  
compare_roc_curves(taus[[2]], mc[[2]], main="Glaciation II", cex=3, cex.lab=3, cex.axis=3, lwd=3,cex.main=2.5)  
dev.off()



