
cap <- function(x, lower = 0, upper = 1) {
  x <- ifelse (x < lower, lower, x)
  x <- ifelse (x > upper, upper, x)
  x
} 

simulate_trials <- function(N = 10, trials = 20, mu_try = 0.5, mu_succed = 0.5) {
  p_try <- cap(rnorm(N, mu_try, 0.3))
  p_succed <- cap(rnorm(N, mu_succed, 0.3))
  tries <- lapply(p_try, rbinom, n = trials, size = 1)
  success <- lapply(p_succed, rbinom, n = trials, size = 1)
  success_matrix <- mapply(function(x, y) x*y, tries, success)
  tries_matrix <- Reduce(function(x,y) cbind(x,y), tries)
  colnames(tries_matrix)<- NULL
  list(tries = tries_matrix, success = success_matrix)
}

summarize_trials <- function(sim_trials) {
  list(colSums(sim_trials$success)/colSums(sim_trials$tries),
       colSums(sim_trials$success)/nrow(sim_trials$success))
}
