require(warningsignals)
load("partIV.Rdat")

taus<-reformat_tau_dists(list(fn_taus, tp_taus, tn_taus, fp_taus))
png("roc_examples.png")
par(mar=c(4,6,4,2))
roc_curve(taus[[1]], lwd=4, cex.axis=2, cex.lab=2)
roc_curve(taus[[2]], add=TRUE, col="blue", lwd=4)
roc_curve(taus[[3]], add=TRUE, col="red", lwd=4)
roc_curve(taus[[4]], add=TRUE, col="green", lwd=4)
legend("bottomright", c("critical","critical", "stable", "stable"), lty=1, col=c("black", "blue", "red", "green"), lwd=4)

