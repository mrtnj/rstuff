source("records_functions.R")

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


## Priors

intercept_prior <- function(n) abs(rnorm(n, 5, 1))
sd_prior <- function(n) abs(rnorm(n, 0, 1))
improvement_prior <- function(n) rnorm(n, 0, 0.1)

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



## ABC runs
print("improvement")
improvement_model <- abc_linear(priors) %>% evaluate_abc(priors = priors)
save(improvement_model, file = "improvement.Rdata")

print("no improvement")
no_improvement_model <- abc_linear(priors_no_improvement) %>% evaluate_abc(priors = priors_no_improvement)
save(no_improvement_model, file = "no_improvement.Rdata")
rm(no_improvement_model)

print("curved improvement")
curved_improvement_model <- abc_linear(priors_curved_improvement) %>% evaluate_abc(priors = priors_curved_improvement)
save(curved_improvement_model, file = "curved_improvement.Rdata")
rm(curved_improvement_model)

print("random improvement")
random_improvement_model <- abc_random(priors_random_improvement) %>% evaluate_abc(priors = priors_random_improvement)
save(random_improvement_model, file = "random_improvement.Rdata")
rm(random_improvement_model)

print("saturating improvement")
saturating_improvement_model <- abc_saturating(priors_saturating_improvement) %>% evaluate_abc(priors = priors_saturating_improvement)
save(saturating_improvement_model, file = "saturating_improvement.Rdata")
rm(saturating_improvement_model)

print("logistic improvement")
logistic_improvement_model <- abc_logistic(priors_logistic_improvement) %>% evaluate_abc(priors = priors_logistic_improvement)
save(logistic_improvement_model, file = "logistic_improvement.Rdata")
