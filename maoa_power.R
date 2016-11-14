
## Power simulations for Caspi et al 2002, MAOA GxE candidate gene study

library(plyr)
library(broom)
library(magrittr)

data <- data.frame(genotype = c("low", "low", "low", "high", "high", "high"),
                   maltreatment = c("no", "probable", "severe", "no", "probable", "severe"),
                   N = c(108, 42, 13, 180, 79, 20),
                   conduct_fraction = c(0.25, 0.35, 0.85, 0.25, 0.3, 0.45))
data$conduct_disorder <- round(data$conduct_fraction * data$N)
data$group <- paste(data$genotype, data$maltreatment)

(odds_population <- sum(data$conduct_disorder)/sum(data$N - data$conduct_disorder))
(p_population <- sum(data$conduct_disorder)/sum(data$N))

(odds_low_severe <- data$conduct_disorder[data$group == "low severe"] /
  (data$N[data$group == "low severe"] - data$conduct_fraction[data$group == "low severe"]))

(odds_high_severe <- data$conduct_disorder[data$group == "high severe"] /
  (data$N[data$group == "high severe"] - data$conduct_fraction[data$group == "high severe"]))


expanded_data <- ddply(data, c("genotype", "maltreatment"), function(x) {
  data.frame(genotype = rep(x$genotype, x$N),
             maltreatment = rep(x$maltreatment, x$N),
             conduct_disorder = c(rep(1, x$conduct_disorder),
                                  rep(0, x$N - x$conduct_disorder)))
})
expanded_data$exposure <- ifelse(expanded_data$maltreatment == "no", "no", "yes")

ddply(subset(expanded_data, genotype == "low"), c("exposure", "genotype"), summarise,
      conduct_disorder = sum(conduct_disorder), n = length(exposure))

model <- glm(conduct_disorder ~ genotype * maltreatment,
             expanded_data, family = binomial(link = "logit"))

## Odds ratios

(estimates <- data.frame(exp(cbind(coef(model), confint(model)))))
estimates$coef <- rownames(estimates)
colnames(estimates) <- c("OR", "lower", "upper", "coef")


## Caspi 2002 OR 2.8 (1.42 - 5.74)
## Caspi maltreatment probable 1.3, severe 2.5
## Rautiainen 2016 GWAS 1.6
## Ficks & Waldman 2013 1.2 Metaanalysis of marginal MAOA 

simplified_model <- glm(conduct_disorder ~ exposure,
                        subset(expanded_data, genotype == "low"), family = binomial(link = "logit"))


(simplified_estimates <- exp(cbind(coef(simplified_model), confint(simplified_model))))

simulate_data <- function(OR, p_exposure = 55/108, N = 55+108) {
  x <- rbinom(N, 1, p_exposure)
  exposure <- ifelse(x == 1, "yes", "no")
  z <- log(0.33) + log(OR) * x
  p <- 1/(1 + exp(-z)) 
  y <- rbinom(N, 1, p) 
  data.frame(exposure, conduct_disorder = y)
}


fake_data <- function(OR, N = 1000) {
  replicate(N, simulate_data(OR = OR), simplify = FALSE)
}

fake_glms <- function(fake_data) {
  results <- ldply(fake_data, function(x) {
    tidy(glm(conduct_disorder ~ exposure, x, family = binomial(link = "logit")))
  })
  subset(results, term == "exposureyes")
}

power_summary <- function(fake_glms, true_OR) {
  significant <- subset(fake_glms, p.value < 0.05)
  data.frame(power = nrow(significant)/nrow(fake_glms),
             exaggeration = exp(mean(significant$estimate))/true_OR,
             sign_error = nrow(subset(significant, estimate < 0))/nrow(fake_glms))
}


power_simulations <- ldply(seq(from = 1.1, to = 3, by = 0.1),
  function(x) fake_data(x) %>% fake_glms %>% power_summary(true_OR = x))
power_simulations$OR <- seq(from = 1.1, to = 3, by = 0.1)

melted <- melt(power_simulations[, -2], id.vars = "OR")

power_plot <- qplot(x = OR, y = value, data = melted, colour = variable) +
  ylim(0, 1) + geom_smooth(se = FALSE)

exaggeration_plot <- qplot(x = OR, y = exaggeration, data = power_simulations) +
  geom_smooth(se = FALSE)
