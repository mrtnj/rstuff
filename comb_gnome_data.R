
## Generate the silly comb gnome dataset

growth <- function(g0, a, t) {

  g0*exp(a*t)

}
n <- 100
group <- c(rep("green", n/2), rep("pink", n/2))
treatment <- sample(c("control", "pixies"), length(group), replace = TRUE)
mass0 <- as.numeric(group=="green")*40 + rnorm(n,100,50)

rates <- rnorm(n, 0.01, 0.005) +
  as.numeric(group == "green") * rnorm(n, 0.01, 0.005) +
  as.numeric(treatment == "pixies") * rnorm(n, 0.03, 0.01)

data <- data.frame(group = group, treatment = treatment,
  mass0 = mass0, mass10 = growth(mass0, rates, 10),
  mass25 = growth(mass0, rates, 25), mass50 = growth(mass0, rates, 50))

write.csv(data, file = "comb_gnome_data.csv",
          quote = FALSE, row.names = FALSE)
