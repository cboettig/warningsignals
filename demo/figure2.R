### Social report
require(socialR)
script <- "figure2.R"
gitaddr <- gitcommit(script) # ok to do last since quick-run script
tags="warningsignals, stochpop"


require(warningsignals)
data(ibms)
data(deuterium)
data(drake)

X <- list(Stable=ibm_stable, Deteriorating=ibm_critical, Glaciation=deuterium[[3]], Daphnia=drake_deterior$H6)

png("indicators.png", width=4.6, units="in", height=2.3, res=1200)
all_indicators(X)
dev.off()

upload("indicators.png", script=script, gitaddr=gitaddr, tags=tags)
