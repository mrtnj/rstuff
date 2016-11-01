## Long jump records

library(reshape2)
library(ggplot2)
library(plyr)

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

mens_high_jump_records <- read.csv("~/mens_high_jump_records.txt", comment.char="#")

data_records <- data.frame(year = 1901:2016,
                           meters = NA)
record <- data_records$meters[1] <- mens_high_jump_records$meters[1]
for (i in 2:nrow(data_records)) {
  record <- max(record, subset(mens_high_jump_records, 
                               year == data_records$year[i])$meters)
  data_records$meters[i] <- record
}

data_seasons <- read.csv("~/mens_high_jump_year_best.txt", comment.char="#")


real_plot <- ggplot() +
  geom_line(aes(x = year, y = meters), data = data_records) +
  geom_point(aes(x = year, y = meters), data = data_seasons)


## priors

tries_per_year <- 100
intercept_prior <- function(n) abs(rnorm(n, 7, 1))
sd_prior <- function(n) abs(rnorm(n, 0, 1))
improvement_prior <- function(n) rnorm(n, 0, 0.1)



## ABC
nsim <- 10000
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
                                          0.1 * improvement_prior(nsim))

run_abc <- function(priors, cutoff = 4, use_seasons = FALSE) {
  sims <- alply(priors, 1, function(x)
    simulate_records(intercept = x$intercept,
    sd = x$sd,
    improvement = x$improvement,
    improvement_squared = x$improvement_squared))


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
  
  cutoff <- quantile(summary_statistics, 0.01)
  posterior <- priors[which(summary_statistics < cutoff),]
  accepted_sims <- sims[which(summary_statistics < cutoff)]
  sim_records <- llply(accepted_sims, function(x) x$record)
  sim_records <- Reduce(function(x,y) cbind(x, y), sim_records)
  sim_season_best <- llply(accepted_sims, function(x) x$season_best)
  sim_season_best <- Reduce(function(x,y) cbind(x, y), sim_season_best)
  colnames(sim_records) <- 1:ncol(sim_records)
  colnames(sim_season_best) <- 1:ncol(sim_season_best)
  
  list(posterior, sim_records, sim_season_best)
}

##improvement_model <- run_abc(priors, use_seasons = TRUE)
##no_improvement_model <- run_abc(priors_no_improvement, use_seasons = TRUE)
##curved_improvement_model <- run_abc(priors_curved_improvement, use_seasons = TRUE)

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