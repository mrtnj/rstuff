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




nsim <- 1e4

## ABC runs
print("improvement")
priors <- priors_improvement(nsim)
improvement_model <- abc(priors, simulate_records) %>% evaluate_abc(priors = priors)
save(improvement_model, file = "improvement.Rdata")

print("no improvement")
priors_no_improvement <- priors_no_improvement(nsim)
no_improvement_model <- abc(priors_no_improvement, simulate_records) %>% evaluate_abc(priors = priors_no_improvement)
save(no_improvement_model, file = "no_improvement.Rdata")


print("random improvement")
priors_random_improvement <- priors_random_improvement(nsim)
random_improvement_model <- abc(priors_random_improvement, simulate_records_random) %>% evaluate_abc(priors = priors_random_improvement)
save(random_improvement_model, file = "random_improvement.Rdata")


print("saturating improvement")
priors_saturating_improvement <- priors_saturating_improvement(nsim)
saturating_improvement_model <- abc(priors_saturating_improvement, simulate_records_saturating) %>% evaluate_abc(priors = priors_saturating_improvement)
save(saturating_improvement_model, file = "saturating_improvement.Rdata")


print("logistic improvement")
priors_logistic_improvement <- priors_logistic_improvement(nsim)
logistic_improvement_model <- abc(priors_logistic_improvement, simulate_records_logistic) %>% evaluate_abc(priors = priors_logistic_improvement)
save(logistic_improvement_model, file = "logistic_improvement.Rdata")
