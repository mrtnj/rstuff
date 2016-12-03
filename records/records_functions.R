## Long jump records

library(reshape2)
library(ggplot2)
library(plyr)
library(magrittr)


## Simulation functions that work on a set of prior parameters, and return simulated values.

simulate_records <- function(p,
                             years = 116,
                             tries_per_year = 100) {
  jumps <- numeric(years)
  record <- numeric(length(jumps))
  jumps[1] <- max(rnorm(tries_per_year, p$intercept, p$sd))
  record[1] <- jumps[1]
  for (j in 2:length(jumps)) {
    jumps_this_year <- rnorm(tries_per_year,
                             p$intercept + p$improvement * (j - 1) +
                               p$improvement_squared * (j - 1)^2, p$sd)
    jumps[j] <- max(jumps_this_year)
    if (jumps[j] > record[j - 1])
      record[j] <- jumps[j]
    else
      record[j] <- record[j - 1]
  }
  list(record = record, season_best = jumps)
}


simulate_records_random <- function(p,
                                    years = 116,
                                    tries_per_year = 100) {
  jumps <- numeric(years)
  record <- numeric(length(jumps))
  improvement <- rbinom(n = years, size = 1, prob = p$improvement_p)
  jumps[1] <- max(rnorm(tries_per_year, p$intercept + improvement[1], p$sd))
  record[1] <- jumps[1]
  for (j in 2:length(jumps)) {
    jumps_this_year <- rnorm(tries_per_year,
                             p$intercept + sum(improvement[1:j]), p$sd)
    jumps[j] <- max(jumps_this_year)
    if (jumps[j] > record[j - 1])
      record[j] <- jumps[j]
    else
      record[j] <- record[j - 1]
  }
  list(record = record, season_best = jumps)
}



simulate_records_saturating <- function(p,
                                        years = 116,
                                        tries_per_year = 100) {
  jumps <- numeric(years)
  record <- numeric(length(jumps))
  mu <- p$mu_max * 1:years / (p$K + (1:years))
  
  jumps[1] <- max(rnorm(tries_per_year, p$intercept + mu[1], p$sd))
  record[1] <- jumps[1]
  for (j in 2:length(jumps)) {
    jumps_this_year <- rnorm(tries_per_year,
                             p$intercept + mu[j], p$sd)
    jumps[j] <- max(jumps_this_year)
    if (jumps[j] > record[j - 1])
      record[j] <- jumps[j]
    else
      record[j] <- record[j - 1]
  }
  list(record = record, season_best = jumps)
}


simulate_records_logistic <- function(p,
                                      years = 116,
                                      tries_per_year = 100) {
  jumps <- numeric(years)
  record <- numeric(length(jumps))
  mu <- p$mu_max / (1 + exp(-p$a * (1:years - p$b)))
  
  jumps[1] <- max(rnorm(tries_per_year, p$intercept + mu[1], p$sd))
  record[1] <- jumps[1]
  for (j in 2:length(jumps)) {
    jumps_this_year <- rnorm(tries_per_year,
                             p$intercept + mu[j], p$sd)
    jumps[j] <- max(jumps_this_year)
    if (jumps[j] > record[j - 1])
      record[j] <- jumps[j]
    else
      record[j] <- record[j - 1]
  }
  list(record = record, season_best = jumps)
}


## Functions for generation prior values

intercept_prior <- function(n) abs(rnorm(n, 5, 1))
sd_prior <- function(n) abs(rnorm(n, 0, 1))
improvement_prior <- function(n) rnorm(n, 0, 0.1)

priors_improvement <- function(nsim)
  data.frame(intercept = intercept_prior(nsim),
             sd = sd_prior(nsim),
             improvement = improvement_prior(nsim),
             improvement_squared = 0)

priors_no_improvement <- function(nsim)
  data.frame(intercept = intercept_prior(nsim),
             sd = sd_prior(nsim),
             improvement = 0,
             improvement_squared = 0)

priors_curved_improvement <- function(nsim)
  data.frame(intercept = intercept_prior(nsim),
             sd = sd_prior(nsim),
             improvement = improvement_prior(nsim),
             improvement_squared = improvement_prior(nsim))

priors_random_improvement <- function(nsim)
  data.frame(intercept = intercept_prior(nsim),
             sd = sd_prior(nsim),
             improvement = 10 * improvement_prior(nsim),
             improvement_p = runif(nsim, 0, 1))

priors_saturating_improvement <- function(nsim)
  data.frame(intercept = intercept_prior(nsim),
             sd = sd_prior(nsim),
             mu_max = improvement_prior(nsim),
             K = runif(nsim, 0, 1000))

priors_logistic_improvement <- function(nsim)
  data.frame(intercept = intercept_prior(nsim),
             sd = sd_prior(nsim),
             mu_max = improvement_prior(nsim),
             a = rnorm(nsim, 0, 10),
             b = rnorm(nsim, 0, 10))



## Compare simulated data to real data, keep accepted, and return relevant parts.

evaluate_abc <- function(sims, priors, cutoff = 3) {
  euclidian_distance <- function(p, q) sqrt(sum((p - q)^2))

  summary_statistics <- unlist(llply(sims, function(x)
    euclidian_distance(p = x$record, q = data_records$meters)))

  passed <- which(summary_statistics < cutoff)
  if (length(passed) > 0) {
    posterior <- priors[passed,]
    accepted_sims <- sims[passed]
    accepted_summary_statistics <- summary_statistics[passed]
    sim_records <- llply(accepted_sims, function(x) x$record)
    sim_records <- data.frame(Reduce(function(x,y) cbind(x, y), sim_records))
    sim_season_best <- llply(accepted_sims, function(x) x$season_best)
    sim_season_best <- data.frame(Reduce(function(x,y) cbind(x, y), sim_season_best))
    colnames(sim_records) <- 1:ncol(sim_records)
    colnames(sim_season_best) <- 1:ncol(sim_season_best)
  
    results <- list(posterior, sim_records, sim_season_best, accepted_summary_statistics)
  } else {
    results <- NULL
  }
  results
}

## ABC run function: just shoves parameters into a simulation function.

abc <- function(priors, simulation_fun) {
  alply(priors, 1, simulation_fun)
}

## Plot simulated data and real data together.

posterior_plot <- function(sim_data) {
  sim_data$year <- 1:nrow(sim_data)
  melted_sim <- melt(sim_data, id.vars = "year")
  colnames(melted_sim) <- c("year", "replicate", "record")
  melted_sim$year <- melted_sim$year + 1900
  
  ggplot() +
    geom_line(aes(x = year, y = record, group = replicate),
              alpha = 1/5, data = melted_sim) +
    geom_line(aes(x = year, y = meters), data = data_records,
              colour = "red") + theme_bw()
}


## Calculate how often, in a simulated dataset, a new record happens.

number_of_records <- function(sim_data) {
  sim_data$year <- 1:nrow(sim_data)
  melted_sim <- melt(sim_data, id.vars = "year")
  colnames(melted_sim) <- c("year", "replicate", "record")
  
  ddply(melted_sim, "replicate", function(x) {
    length(unique(x$record))
  })
}
