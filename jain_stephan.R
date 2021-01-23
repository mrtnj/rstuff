
library(gganimate)
library(ggplot2)
library(tidyr)

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
  
  p_df <- data.frame(p)
  colnames(p_df) <- paste("p", 1:ncol(p_df), sep = "")
  p_df$t <- 1:nrow(p_df)
  p_df
}  


## Simulate approaching the optimum

s <- 1
gamma <- c(1, 0.1)
z_prime <- 0 ## We start at optimum = 0
mu <- 1e-4
n_gen <- 1000

p0 <- c(0.01, 0.1)
  
p <- run_steps(s, gamma, p0, z_prime, mu, n_gen)

plot_approach_2d <- function(p) {
  qplot(x = p1, y = p2, data = p) +
  xlim(0, 1) +
  ylim(0, 1) +
  theme_bw()
}


plot_example <- plot_approach_2d(p)


animation_example <-
  qplot(x = p1, y = p2,
        data = p[c(1:9, seq(from = 10, to = 1000, by = 10)),],
        size = I(5)) +
    xlim(0, 1) +
    ylim(0, 1) +
    theme_bw(base_size = 16)+
    theme(panel.grid = element_blank()) +
    transition_states(t) +
    shadow_trail(alpha = 0.3)

  
anim_save("jain_stephan_approach.gif", animation = animation_example)


## Starting loci at equilibrium

get_equilibrium_frequencies <- function(mu, s, gamma) {
  c(0.5,
    0.5 * (1 + sqrt(1 - 8 * mu / (s * gamma^2))),
    0.5 * (1 - sqrt(1 - 8 * mu / (s * gamma^2))))
}

threshold <- function(mu, s) sqrt(8 * mu / s)

threshold(1e-4, 1)


## Trajectory after shift in optimum

(eq0.05 <- get_equilibrium_frequencies(1e-4, 1, 0.05))

p0.05 <- run_steps(s = s,
                   gamma = c(0.05, 0.05),
                   p0 = c(eq0.05[2], eq0.05[3]),
                   z_prime = 0.1,
                   mu = 1e-4,
                   n_gen = 1000)

pheno_mean(p = c(p0.05$p1[nrow(p0.05)],
                 p0.05$p2[nrow(p0.05)]),
           gamma = c(0.05, 0.05))

p0.01 <- run_steps(s = s,
                   gamma = c(0.01, 0.01),
                   p0 = c(0.5, 0.5),
                   z_prime = 0.1,
                   mu = 1e-4,
                   n_gen = 1000)

pheno_mean(p = c(p0.01$p1[nrow(p0.01)],
                 p0.01$p2[nrow(p0.01)]),
           gamma = c(0.01, 0.01))

plot_approach0.05 <- plot_approach_2d(p0.05)

plot_approach0.01 <- plot_approach_2d(p0.01)


## Animations

animation0.05 <-
  qplot(x = p1, y = p2,
        data = p0.05[seq(from = 10, to = 1000, by = 10),],
        size = I(5)) +
  xlim(0, 1) +
  ylim(0, 1) +
  theme_bw(base_size = 16)+
  theme(panel.grid = element_blank()) +
  transition_states(t) +
  shadow_trail(alpha = 0.3)

animation0.01 <-
  qplot(x = p1, y = p2,
        data = p0.01[seq(from = 10, to = 1000, by = 10),],
        size = I(5)) +
  xlim(0, 1) +
  ylim(0, 1) +
  theme_bw(base_size = 16)+
  theme(panel.grid = element_blank()) +
  transition_states(t) +
  shadow_trail(alpha = 0.3)


anim_save("jain_stephan0.05.gif", animation = animation0.05)

anim_save("jain_stephan0.01.gif", animation = animation0.01)



