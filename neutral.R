
## Simulate genetic drift in a Wright-Fisher scenario

library(reshape2)
library(ggplot2)

gen <- 1000 ## generations
n_sim <- 10 ## simulations
N <- 500 ## population size

sample_gametes <- function(p) {
  gametes <- table(rbinom(2 * N, 1, p))
  new_p <- gametes[2]/(gametes[1] + gametes[2])
  new_p
}

sims <- replicate(n_sim, {
  p <- numeric(gen)
  p[1] <- 0.5
  for (i in 1:(gen - 1)) {
    p[i + 1] <- sample_gametes(p[i])
  }
  p
})

melted <- melt(sims)
colnames(melted) <- c("generation", "replicate", "p")
trajectory_plot <- qplot(x = generation, y = p, group = factor(replicate),
                         data = melted, geom = "line", alpha = I(0.5)) + theme_bw()