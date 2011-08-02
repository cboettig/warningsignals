### Social report
require(socialR)
script <- "figure2.R"
gitaddr <- gitcommit(script) # ok to do last since quick-run script
tags="warningsignals, stochpop"


require(warningsignals)
data(ibms)
data(deuterium)
data(drake)

X <- list("(a) Stable"=ibm_stable, "(b) Deteriorating"=ibm_critical, "(c) Daphnia"=drake_deterior$H6, "(d) Glaciation III"=deuterium[[3]])

png("Fig2.pdf", width=6, units="in", height=4.6)
all_indicators(X, indicators=c("Var", "Autocor", "Skew", "CV"), method="kendall", pval=FALSE)
dev.off()

#upload("indicators.png", script=script, gitaddr=gitaddr, tags=tags)
