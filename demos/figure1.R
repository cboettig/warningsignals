# figure1.R
require(warningsignals)

data(ibms)
all_indicators(list(deteriorating=ibm_critical, stable=ibm_stable))

deterior_m<-fit_models(ibm_critical, "LSN")
constant_m<-fit_models(ibm_stable, "LSN")
deterior_taus <- bootstrap_tau(deterior_m$X, deterior_m$const, 
                               deterior_m$timedep, indicators = indicators,
                               method="kendall", nboot=nboot, cpu=cpu)
constant_taus <- bootstrap_tau(constant_m$X, constant_m$const,
                               constant_m$timedep, indicators = indicators,
                               method="kendall", nboot=nboot, cpu=cpu)

deterior_mc <- montecarlotest(deterior_m$const, deterior_m$timedep, 
                              cpu=cpu, nboot=nboot)

constant_mc <- montecarlotest(constant_m$const, constant_m$timedep, 
                              cpu=cpu, nboot=nboot)













data(drake)

all_indicators(list(Daphnia=drake$data[["H6"]]), indicators=c("Var", "Autocor"),
               cex.axis=.8, cex.lab=.8, lwd=.5)

plot.bootstrap_tau(Daphnia=drake$taus[["H6"]][1:3], cex.axis=.8, cex.lab=.8, 
                   show_p=FALSE, ylim=c(0,2.8), yaxp = c(0, 3, 3), xaxp=c(-1,1,5))


plot(drake$mc,show_text = c("p","power"), xlab="", main="",  cex.lab=1, ylim=c(0,.4), xlim=c(0,80))

  mtext(data_names[1], NORTH<-3, cex=par()$cex.lab, line=1) ## data labels on top row
  mtext("Probability density", WEST<-2, line=2, cex=par()$cex.lab) ## statistic name on first column
  mtext("Likelihood Ratio", SOUTH<-1, line=2, cex=par()$cex.lab) ## x-axis label

  
