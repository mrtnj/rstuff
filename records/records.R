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



no_improvement <- replicate(50, simulate_records(improvement = 0)$record)
with_improvement <- replicate(50, simulate_records()$record)

melted_no_improvement <- melt(no_improvement)
melted_improvement <- melt(with_improvement)
colnames(melted_no_improvement) <- c("year", "replicate", "record")
colnames(melted_improvement) <- c("year", "replicate", "record")
melted_no_improvement$improvement <- "no improvement"
melted_improvement$improvement <- "improvement"

combined <- rbind(melted_no_improvement, melted_improvement)

simulated_plot <- qplot(x = year, group = replicate, y = record, alpha = I(1/5),
      data = combined, facets = ~ improvement, geom = "line") + theme_bw()


## Data

mens_long_jump_records <- read.csv("mens_long_jump_records.txt", comment.char="#")

data_records <- data.frame(year = 1901:2016,
                           meters = NA)
record <- data_records$meters[1] <- mens_long_jump_records$meters[1]
for (i in 2:nrow(data_records)) {
  record <- max(record, subset(mens_long_jump_records, 
                               year == data_records$year[i])$meters)
  data_records$meters[i] <- record
}

data_seasons <- read.csv("mens_long_jump_year_best.txt", comment.char="#")


real_plot <- ggplot() +
  geom_line(aes(x = year, y = meters), data = data_records) +
  geom_point(aes(x = year, y = meters), data = data_seasons)


## priors

intercept_prior <- function(n) abs(rnorm(n, 5, 1))
sd_prior <- function(n) abs(rnorm(n, 0, 1))
improvement_prior <- function(n) rnorm(n, 0, 0.1)



## ABC
nsim <- 1e6
priors <- data.frame(intercept = intercept_prior(nsim),
                     sd = sd_prior(nsim),
                     improvement = improvement_prior(nsim),
                     improvement_squared = 0)
priors_no_improvement <- data.frame(intercept = intercept_prior(nsim),
                                    sd = sd_prior(nsim),
                                    improvement = 0,
                                    improvement_squared = 0)
priors_curved_improvement <- data.frame(intercept = intercept_prior(nsim),
                                        sd = sd_prior(nsim),
                                        improvement = improvement_prior(nsim),
                                        improvement_squared = 
                                          improvement_prior(nsim))
priors_random_improvement <- data.frame(intercept = intercept_prior(nsim),
                                        sd = sd_prior(nsim),
                                        improvement = 10 * improvement_prior(nsim),
                                        improvement_p = runif(nsim, 0, 1))

priors_saturating_improvement <- data.frame(intercept = intercept_prior(nsim),
                                            sd = sd_prior(nsim),
                                            mu_max = improvement_prior(nsim),
                                            K = runif(nsim, 0, 1000))

priors_logistic_improvement <- data.frame(intercept = intercept_prior(nsim),
                                          sd = sd_prior(nsim),
                                          mu_max = improvement_prior(nsim),
                                          a = rnorm(nsim, 0, 10),
                                          b = rnorm(nsim, 0, 10))

evaluate_abc <- function(sims, priors, cutoff = 3, use_seasons = FALSE) {
  euclidian_distance <- function(p, q) sqrt(sum((p - q)^2))

  if (!use_seasons) {
    summary_statistics <- unlist(llply(sims, function(x)
      euclidian_distance(p = x$record, q = data_records$meters)))
  } else {
    sim_records_seasons <- llply(sims, function(x) {
      c(x$record, x$season_best[61:115])
    })
    summary_statistics <- unlist(llply(sim_records_seasons,
                                       function (x)
                                       euclidian_distance(p = x,
                                       q = c(data_records$meters,
                                             data_seasons$meters))))
  }
  
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

improvement_model <- abc_linear(priors) %>% evaluate_abc(priors = priors,use_seasons = FALSE)
print("no improvement")
no_improvement_model <- abc_linear(priors_no_improvement) %>% evaluate_abc(priors = priors_no_improvement, use_seasons = FALSE)
save(no_improvement)
print("curved improvement")
curved_improvement_model <- abc_linear(priors_curved_improvement) %>% evaluate_abc(priors = priors_curved_improvement, use_seasons = FALSE)
print("random improvement")
random_improvement_model <- abc_random(priors_random_improvement) %>% evaluate_abc(priors = priors_random_improvement, use_seasons = FALSE)
print("saturating improvement")
saturating_improvement_model <- abc_saturating(priors_saturating_improvement) %>% evaluate_abc(priors = priors_saturating_improvement, use_seasons = FALSE)
print("logistic improvement")
logistic_improvement_model <- abc_logistic(priors_logistic_improvement) %>% evaluate_abc(priors = priors_logistic_improvement, use_seasons = FALSE)

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

##posterior_plot1 <- posterior_plot(improvement_model[[2]])
##posterior_plot2 <- posterior_plot(no_improvement_model[[2]])