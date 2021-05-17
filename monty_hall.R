
## Generalised Monty Hall simulation

library(assertthat)
library(dplyr)

## Wrap sample into a function that avoids the "convenience"
## behaviour that happens when the length of x is one

sample_safer <- function(to_sample, n) {
  assert_that(n <= length(to_sample))
  if (length(to_sample) == 1)
    return(to_sample)
  else {
    return(sample(to_sample, n))
  }
}


## Simulate a generalised Monty Hall situation with
## w prizes, d doors and o doors that are opened.

sim_choice <- function(w, d, o) {
  assert_that(w < d - o) ## There has to be less prizes than unopened doors
  wins <- rep(1, w)
  losses <- rep(0, d - w)
  doors <- c(wins, losses)
  
  ## Pick a door
  choice <- sample_safer(1:d, 1)
  
  ## Doors that can be opened
  to_open_from <- which(doors == 0)
  
  ## Chosen door can't be opened
  to_open_from <- to_open_from[to_open_from != choice]
  
  ## Doors to open
  to_open <- sample_safer(to_open_from, o)
  
  ## Switch to one of the remaining doors
  possible_switches <- setdiff(1:d, c(to_open, choice))
  choice_after_switch <- sample_safer(possible_switches , 1)
  
  result_hold <- doors[choice]
  result_switch <- doors[choice_after_switch]
  c(result_hold,
    result_switch)
}


## Formulas for probabilities

mh_formula <- function(w, d, o) {
  assert_that(w < d - o) ## There has to be less prizes than unopened doors
  
  p_win_switch <- w/d * (w - 1)/(d - o - 1) + (1 - w/d) * w / (d - o - 1) 
  p_win_hold <- w/d
  c(p_win_hold,
    p_win_switch)
}


## Standard Monty Hall

mh_formula(1, 3, 1)
mh <- replicate(1000, sim_choice(1, 3, 1))
rowSums(mh)/ncol(mh)


## vary d, vary w and o

d <- seq(from = 1, to = 100)
w <- seq(from = 1, to = 100)
o <- seq(from = 1, to = 100)


combinations <- expand.grid(w = w, o = o, d = d)
combinations <- filter(combinations, w < d - o)
n_combinations <- nrow(combinations)

combinations$p_win_hold <- 0
combinations$p_win_switch <- 0

for (combination_ix in 1:n_combinations) {
  p <- mh_formula(d = combinations$d[combination_ix],
                  w = combinations$w[combination_ix],
                  o = combinations$o[combination_ix])
  combinations$p_win_hold[combination_ix] <- p[1]
  combinations$p_win_switch[combination_ix] <- p[2]
}

