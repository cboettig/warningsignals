# file fishcollapse.R
# Author Carl Boettiger <cboettig@gmail.com>
# Date: 10 Nov 2011
# Description: an example trying to detect early warning signals
#  in data from fisheries collapses


require(warningsignals)
require(ggplot2)

# Load the data #
# ------------- #


# humbolt current data from Sea Around Us
humboldt <- read.csv("../data/humboldt.csv")
cod <- read.csv("../data/noaa_cod.csv") 
# Sea Around Us Scotian Shelf (George's Banks)
scotia <- read.csv("../data/sau_scotia.csv")

# Visualize data #
# ------------- #
dat_humboldt <- melt(humboldt, id="Year")
p_humboldt <- ggplot(dat_humboldt, aes(Year, value, fill=variable)) + 
              geom_area()

dat_scotia <- melt(scotia, id="Year")
p_scotia <- ggplot(dat_scotia, aes(Year, value, fill=variable)) + 
            geom_area()

#print(p_humboldt)
print(p_scotia)


cod <- subset(dat_scotia, Year < 1992 & variable == "Atlantic.cod")
cod.ts <- ts(cod[["value"]], start=1950, end=1991, deltat=1)



m <- fit_models(cod.ts, "LSN")

# decreasing stability? 
m$timedep$pars$m < 0 


## analysis of summary statistics
#taus <- reformat_tau_dists(bootstrap_tau(m$X, m$const, m$timedep, cpu=4, nboot=20))
mc <- remove_unconverged(montecarlotest(m$const, m$timedep, cpu=4, nboot=20))
plot(mc, main="Warning signals in cod?")


