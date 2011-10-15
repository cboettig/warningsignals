N <- numeric(100)
N[1] <- 99

r <- 4
K <- 100

f <- function(N) r*N*(1-N/K)

for(t in 1:99)
	N[t+1] = f(N[t])

png("chaos.png")
plot(1:100, N)
dev.off()

require(socialR)
upload("chaos.png", script="chaoticseries.R", tags="stochpop")

