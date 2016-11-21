## Long jump records

library(reshape2)
library(ggplot2)
library(plyr)
library(magrittr)

simulate_records <- function(years = 116,
                             tries_per_year = 100,
                             intercept = 7,
                             sd = 0.4,
                             improvement = 0.01,
                             improvement_squared = 0) {
  jumps <- numeric(years)
  record <- numeric(length(jumps))
  jumps[1] <- max(rnorm(tries_per_year, intercept, sd))
  record[1] <- jumps[1]
  for (j in 2:length(jumps)) {
    jumps_this_year <- rnorm(tries_per_year,
                             intercept + improvement * (j - 1) +
                               improvement_squared * (j - 1)^2, sd)
    jumps[j] <- max(jumps_this_year)
    if (jumps[j] > record[j - 1])
      record[j] <- jumps[j]
    else
      record[j] <- record[j - 1]
  }
  list(record = record, season_best = jumps)
}


simulate_records_random <- function(years = 116,
                                    tries_per_year = 100,
                                    intercept,
                                    sd,
                                    improvement,
                                    improvement_p) {
  jumps <- numeric(years)
  record <- numeric(length(jumps))
  improvement <- rbinom(n = years, size = 1, prob = improvement_p)
  jumps[1] <- max(rnorm(tries_per_year, intercept + improvement[1], sd))
  record[1] <- jumps[1]
  for (j in 2:length(jumps)) {
    jumps_this_year <- rnorm(tries_per_year,
                             intercept + sum(improvement[1:j]), sd)
    jumps[j] <- max(jumps_this_year)
    if (jumps[j] > record[j - 1])
      record[j] <- jumps[j]
    else
      record[j] <- record[j - 1]
  }
  list(record = record, season_best = jumps)
}



simulate_records_saturating <- function(years = 116,
                                       tries_per_year = 100,
                                       intercept,
                                       sd,
                                       mu_max,
                                       K) {
  jumps <- numeric(years)
  record <- numeric(length(jumps))
  mu <- mu_max * 1:years / (K + (1:years))
  
  jumps[1] <- max(rnorm(tries_per_year, intercept + mu[1], sd))
  record[1] <- jumps[1]
  for (j in 2:length(jumps)) {
    jumps_this_year <- rnorm(tries_per_year,
                             intercept + mu[j], sd)
    jumps[j] <- max(jumps_this_year)
    if (jumps[j] > record[j - 1])
      record[j] <- jumps[j]
    else
      record[j] <- record[j - 1]
  }
  list(record = record, season_best = jumps)
}


simulate_records_logistic <- function(years = 116,
                                      tries_per_year = 100,
                                      intercept,
                                      sd,
                                      mu_max,
                                      a,
                                      b) {
  jumps <- numeric(years)
  record <- numeric(length(jumps))
  mu <- mu_max / (1 + exp(-a * (1:years - b)))
  
  jumps[1] <- max(rnorm(tries_per_year, intercept + mu[1], sd))
  record[1] <- jumps[1]
  for (j in 2:length(jumps)) {
    jumps_this_year <- rnorm(tries_per_year,
                             intercept + mu[j], sd)
    jumps[j] <- max(jumps_this_year)
    if (jumps[j] > record[j - 1])
      record[j] <- jumps[j]
    else
      record[j] <- record[j - 1]
  }
  list(record = record, season_best = jumps)
}


evaluate_abc <- function(sims, priors, cutoff = 3) {
  euclidian_distance <- function(p, q) sqrt(sum((p - q)^2))

  summary_statistics <- unlist(llply(sims, function(x)
    euclidian_distance(p = x$record, q = data_records$meters)))

  posterior <- priors[which(summary_statistics < cutoff),]
  accepted_sims <- sims[which(summary_statistics < cutoff)]
  accepted_summary_statistics <- summary_statistics[which(summary_statistics < cutoff)]
  sim_records <- llply(accepted_sims, function(x) x$record)
  sim_records <- Reduce(function(x,y) cbind(x, y), sim_records)
  sim_season_best <- llply(accepted_sims, function(x) x$season_best)
  sim_season_best <- Reduce(function(x,y) cbind(x, y), sim_season_best)
  colnames(sim_records) <- 1:ncol(sim_records)
  colnames(sim_season_best) <- 1:ncol(sim_season_best)
  
  list(posterior, sim_records, sim_season_best, accepted_summary_statistics)
}


abc_linear <- function(priors) {
  alply(priors, 1, function(x) 
    simulate_records(intercept = x$intercept,
                     sd = x$sd,
                     improvement = x$improvement,
                     improvement_squared = x$improvement_squared))
}

abc_random <- function(priors) {
  alply(priors, 1, function(x)
    simulate_records_random(intercept = x$intercept,
                            sd = x$sd,
                            improvement = x$improvement,
                            improvement_p = x$improvement_p))
}

abc_saturating <- function(priors) {
  alply(priors, 1, function(x)
    simulate_records_saturating(intercept = x$intercept,
                                sd = x$sd,
                                mu_max = x$mu_max,
                                K = x$K))
}

abc_logistic <- function(priors) {
  alply(priors, 1, function(x)
    simulate_records_logistic(intercept = x$intercept,
                              sd = x$sd,
                              mu_max = x$mu_max,
                              a = x$a,
                              b = x$b))
}

posterior_plot <- function(sim_data) {
  melted_sim <- melt(sim_data)
  colnames(melted_sim) <- c("year", "replicate", "record")
  melted_sim$year <- melted_sim$year + 1900
  
  ggplot() +
    geom_line(aes(x = year, y = record, group = replicate),
              alpha = 1/5, data = melted_sim) +
    geom_line(aes(x = year, y = meters), data = data_records,
              colour = "red") + theme_bw()
}

