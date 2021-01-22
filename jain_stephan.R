
library(ggplot2)


## Function for phenotypic mean and allele frequency change

pheno_mean <- function(p, gamma) {
  sum(gamma * (2 * p - 1))
}

allele_frequency_change <- function(s, gamma, p, z_prime, mu) {
  -s * gamma * p * (1 - p) * (pheno_mean(p, gamma) - z_prime) -
    - s * gamma^2 * 0.5 * p * (1 - p) * (1 - p - p) +
    mu * (1 - p - p)
}


## Run the model from starting allele frequencies

run_steps <- function(s, gamma, p0, z_prime, mu, n_gen) {
  l <- length(p0)

  ## Allele frequency vector for each locus
  p <- vector(mode = "list",
              length = l)

  for (i in seq_along(p)) {
    p[[i]] <- numeric(n_gen)
    p[[i]][1] <- p0[i]
  }
  
  
  for (gen in 2:n_gen) {
    for (loc in 1:l) {
      delta_p <- allele_frequency_change(s, gamma[loc], p[[loc]][gen - 1], z_prime, mu)
      p[[loc]][gen] <- p[[loc]][gen - 1] + delta_p
    }
  }
  p
}  


## Simulate approaching the optimum

s <- 1
gamma <- c(1, 0.1)
z_prime <- 0 ## We start at optimum = 0
mu <- 0.01
n_gen <- 100

p0 <- c(0.2, 0.1)
  
p <- run_steps(s, gamma, p0, z_prime, mu, n_gen)
p_df <- data.frame(p)
colnames(p_df) <- c("p1", "p2")

plot_approach <- qplot(x = p1, y = p2, data = p_df) +
  xlim(0, 1) +
  ylim(0, 1) +
  theme_minimal()

