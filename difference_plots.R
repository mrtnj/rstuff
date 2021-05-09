
## Variations of plots for showing mean difference between two groups

library(broom)
library(dabestr)
library(ggbeeswarm)
library(ggplot2)
library(patchwork)


set.seed(20210509)


## Fake data and model

data <- data.frame(group = rep(0:1, 20))
data$response <- 4 + data$group * 2 + rnorm(20)

model <- lm(response ~ factor(group), data = data)
result <- tidy(model)


## Alternative 1: Jitter and difference as separate panels

plot_points <- ggplot() +
  geom_jitter(aes(x = factor(group), y = response),
              data = data,
              width = 0.1) +
  xlab("Group") +
  ylab("Response") +
  theme_bw()

plot_difference <- ggplot() +
  geom_pointrange(aes(x = term, y = estimate,
                      ymin = estimate - 2 * std.error,
                      ymax = estimate + 2 * std.error),
                  data = result) +
  ylim(-5, 5) +
  ylab("Value") +
  xlab("Coefficient") +
  coord_flip() +
  theme_bw()

plot_combined <- (plot_points /
                    plot_difference) +
                  plot_layout(heights = c(2, 1))

offset <- (2 * result$estimate[1] + result$estimate[2])/2
shortest <- result$estimate[2] - 2 * result$std.error[2]
longest <- result$estimate[2] + 2 * result$std.error[2]


## Alternative 2: Points and differences together

plot_both <- plot_points + 
  geom_linerange(aes(ymin = offset - shortest/2,
                     ymax= offset + shortest/2,
                     x = 1.25)) +
  geom_linerange(aes(ymin = offset - longest/2,
                     ymax= offset + longest/2,
                     x = 1.75)) +
  theme_bw()


## Combined for blog post

my_suggestions <- ((plot_points) / plot_difference) + plot_layout(heights = c(2, 1)) |
  plot_both




## Alternative 3: Gardner-Altman with dabestr

bootstrap <- dabest(data,
                    group,
                    response,
                    idx = c("0", "1"),
                    paired = FALSE)

bootstrap_diff <- mean_diff(bootstrap)

plot(bootstrap_diff)




## Alternative 4: simplifed Garnder-Altman

ymin <- min(data$response)
ymax <- max(data$response)


plot_points_ga <- ggplot() +
  geom_quasirandom(aes(x = factor(group), y = response),
                   data = data) +
  geom_hline(yintercept = coef(model)[1],
              linetype = 2) +
  xlab("Group") +
  ylab("Response") +
  theme_bw() +
  scale_y_continuous(limits = c(ymin, ymax))



height_of_plot <- ymax-ymin

group0_fraction <- (coef(model)[1] - ymin)/height_of_plot

diff_min <- - height_of_plot * group0_fraction

diff_max <- (1 - group0_fraction) * height_of_plot

plot_difference_ga <- ggplot() +
  geom_pointrange(aes(x = term, y = estimate,
                      ymin = estimate - 2 * std.error,
                      ymax = estimate + 2 * std.error),
                  data = result[2,]) +
  geom_hline(yintercept = 0,
             linetype = 2) +
  scale_y_continuous(limits = c(diff_min, diff_max)) +
  ylab("Difference") +
  xlab("Comparison") +
  theme_bw()


(plot_points_ga | plot_difference_ga) + plot_layout(widths = c(0.75, 0.25))

