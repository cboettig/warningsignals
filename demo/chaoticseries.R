N <- numeric(100)
N[1] <- 99

r <- 3.9
K <- 100

f <- function(N) r*N*(1-N/K)

for(t in 1:99)
	N[t+1] = f(N[t])

png("chaos.png")
plot(1:100, N)
dev.off()

require(socialR)
upload("chaos.png", script="chaoticseries.R", tags="stochpop")
chaos <- ts(N,start=0, deltat=1)

save(list="chaos", file="chaos2.rda")
