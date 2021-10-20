## Explore expressions for balancing selection on a deleterious allele

library(dplyr)
library(ggplot2)
library(purrr)
library(tibble)

p0 <- 0.1
h <- 0.5
s <- 0.1

p_next <- function(p, s, h) p * (1 - s * h) / (2 * p * (1 - s * h) + (1 - p) * (1 - s))

n_gen <- 1000
p <- numeric(n_gen + 1)
p[1] <- p0

for (gen_ix in 2:(n_gen + 1)) {
  p[gen_ix] <- p_next(p[gen_ix - 1], s, h)
}


p_df <- tibble(gen = 1:100,
               p = p[1:100])

p_eq <- function(s, h) s * (1 - h) / (1 - s * (2 * h - 1))




sim_balancing <- function(n_gen, p0, N, s, h) {
  
  p <- numeric(n_gen)
  p[1] <- p0
  
  for (gen_ix in 2:n_gen) {
    geno <- rbinom(N,
                   size = 2,
                   p = p[gen_ix -1])
    
    geno_selected1 <- geno[geno < 2]
    
    w2 <- ifelse(geno_selected1 == 1,
                 1 - h * s,
                 1 - s)

    mean_w2 <- mean(w2)
    
    geno_selected2 <- sample(geno_selected1,
                             prob = w2/mean_w2,
                             replace = TRUE)
    
    p[gen_ix] <- sum(geno_selected2)/2/length(geno) 
    
  }
  
  data.frame(gen = 1:n_gen,
             p = p)
}

sim <- map_dfr(1:100,
               function(x) sim_balancing(n_gen = 100,
                                         p0 = p0,
                                         N = 10000,
                                         s = s,
                                         h = h),
               .id = "rep")

mean_sim <- summarise(group_by(sim, gen),
                      average = mean(p),
                      lower = quantile(p, 0.05),
                      upper = quantile(p, 0.95))

ggplot() +
  geom_line(aes(x = gen, y = p, group = rep), data = sim, alpha = 1/10) +
  geom_pointrange(aes(x = gen, y = average, ymin = lower, ymax = upper), data = mean_sim) +
  geom_line(aes(x = gen, y = p), data = p_df, colour = "red")




sim_no_balance <- map_dfr(1:100,
                          function(x) sim_balancing(n_gen = 100,
                                                    p0 = p0,
                                                    N = 10000,
                                                    s = 0,
                                                    h = 1),
               .id = "rep")


mean_sim_no_balance <- summarise(group_by(sim_no_balance, gen),
                                 average = mean(p),
                                 lower = quantile(p, 0.05),
                                 upper = quantile(p, 0.95))

ggplot() +
  geom_line(aes(x = gen, y = p, group = rep), data = sim_no_balance, alpha = 1/10) +
  geom_pointrange(aes(x = gen, y = average, ymin = lower, ymax = upper), data = mean_sim_no_balance) +
  geom_line(aes(x = gen, y = p), data = p_df, colour = "red")

