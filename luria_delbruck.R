

culture <- function(generations, mu) {
  n_susceptible <- numeric(generations)
  n_resistant <- numeric(generations)
  n_mutants <- numeric(generations)
  n_susceptible[1] <- 1
  for (i in 1:(generations - 1)) {
    n_mutants[i] <- rbinom(n = 1, size = n_susceptible[i], prob = mu)
    n_susceptible[i + 1] <- 2 * (n_susceptible[i] - n_mutants[i])
    n_resistant[i + 1] <- 2 * (n_resistant[i] + n_mutants[i])
  }
  data.frame(generation = 1:generations,
             n_susceptible,
             n_resistant,
             n_mutants)
}


cultures <- replicate(1000, culture(30, 2e-8), simplify = FALSE)

combined <- Reduce(function (x, y) rbind(x, y), cultures)
combined$culture <- rep(1:1000, each = 30)

resistant_plot <- qplot(x = generation, y = n_resistant, group = culture,
      data = combined, geom = "line", alpha = I(1/10), size = I(1)) + theme_bw()



resistant <- unlist(lapply(cultures, function(x) max(x$n_resistant)))

acquired_resistant <- rbinom(n = 1000, size = 2^29, 2e-8)

resistant_combined <- rbind(transform(data.frame(resistant = acquired_resistant), model = "acquired"),
                            transform(data.frame(resistant = resistant), model = "mutation"))

resistant_histograms <- qplot(x = resistant, data = resistant_combined,bins = 10) +
  facet_wrap(~ model, scale = "free_x")