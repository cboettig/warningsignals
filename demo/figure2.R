### Social report
rm(list=ls())
require(warningsignals)
data(ibms)
data(deuterium)
data(drake)

X <- list("(a) Stable"=ibm_stable, "(b) Deteriorating"=ibm_critical, "(c) Daphnia"=drake_deterior$H6, "(d) Glaciation III"=deuterium[[3]])

cairo_pdf("Fig2.pdf", width=6,  height=4.6)
all_indicators(X, indicators=c("Var", "Autocor", "Skew", "CV"))
dev.off()

