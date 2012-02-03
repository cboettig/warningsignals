rm(list=ls())
## import the saved run results
load("../data/manuscriptData.rda")
require(warningsignals)
require(ggplot2)
require(reshape2)
require(grid)


pow <- drake[[1]] 


  ## PLOT the distributions
  dat <- melt(list(Null=pow$null_dist, Test=pow$test_dist))
  names(dat) <- c("value", "simulation")
  observed <- -2*(loglik(pow$null)-loglik(pow$test))
  p_dist <- ggplot(dat) + 
  geom_density(aes(value, fill=simulation), alpha=.7) + 
  geom_vline(xintercept=observed, lty=2) + 
    opts(title="Likelihood ratio distributions") +
    scale_x_continuous("Deviance") + scale_y_continuous("Probability density")
  ggsave("distribution.png")

# how about boxplot?
  p_box <- ggplot(dat) + geom_boxplot(aes(simulation, value)) + geom_hline(yintercept=observed, lty=2)

  ggsave("boxplot.png")


  # how about beanplot?
  require(beanplot)
  png("beanplot.png")
  beanplot(value ~ simulation, dat, what=c(0,1,0,0))
  dev.off()


  png("violin.png")
  vioplot(pow$null_dist, pow$test_dist)
  dev.off()

  rocdat <- roc_data(pow$null_dist, pow$test_dist)
  p <- ggplot(rocdat) + geom_line(aes(FalsePos, TruePos), lwd=1) + 
    scale_x_continuous("False Positive") + scale_y_continuous("True Positive")
  ggsave("rocplot.png")


require(socialR)
upload("*.png", script="visualize_distributions.R", tag="warningsignals stochpop")
