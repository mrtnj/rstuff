
## Demonstrate different ways to do plyr-like operations with modern tidyverse

## Needs to be loaded first to not conflict with dplyr
library(plyr)

library(broom)
library(dplyr)
library(magrittr)
library(purrr)
library(tidyr)



set.seed(20210807)

n_groups <- 10
group_sizes <- rpois(n_groups, 10)
n <- sum(group_sizes)

fake_data <- tibble(id = 1:n,
                    group = rep(1:n_groups,
                                times = group_sizes),
                    predictor = runif(n, 0, 1))
group_intercept <- rnorm(n_groups, 0, 1)

fake_data$response <- fake_data$predictor * 10 +
  group_intercept[fake_data$group] +
  rnorm(n)


fit_model <- function(data) {
  lm(response ~ predictor, data)
}


## plyr

models <- dlply(fake_data,
                     "group",
                     fit_model)
result <- ldply(models, tidy)

print(result)


## purr without/with pipe

models <- map(split(fake_data,
                    fake_data$group),
                    fit_model)
result <- map_df(models_purrr,
                 tidy,
                 .id = "group")

print(result)


result <- fake_data %>%
  split(.$group) %>%
  map(fit_model) %>%
  map_df(tidy, .id = "group")

print(result)


## List column approach

fake_data_nested <- nest(group_by(fake_data, group),
                         data = c(id, predictor, response))

fake_data_models <- mutate(fake_data_nested,
                           model = map(data,
                                       fit_model),
                           estimates = map(model,
                                           tidy))

result <- unnest(fake_data_models, estimates)

print(result)


fake_data %>% 
  group_by(group) %>% 
  nest(data = c(id, predictor, response)) %>% 
  mutate(model = map(data, fit_model),
         estimates = map(model, tidy)) %>% 
  unnest(estimates) ->
  result

print(result)



## Linear mixed model

library(ggplot2)
library(lme4)

model <- lmer(response ~ (1|group) + predictor,
              fake_data)

lm_coef <- pivot_wider(result,
                       names_from = term,
                       values_from = estimate,
                       id_cols = group)


lmm_coef <- cbind(group = levels(model@flist$group),
                  coef(model)$group)

model_coef <- rbind(transform(lm_coef, model = "lm"),
                    transform(lmm_coef, model = "lmm"))

colnames(model_coef)[2] <- "intercept"

ggplot() +
  geom_point(aes(x = predictor,
                 y = response,
                 colour = factor(group)),
             data = fake_data) +
  geom_abline(aes(slope = predictor,
                  intercept = intercept,
                  colour = factor(group)
                  ,linetype = model),
              data = model_coef) +
  theme_bw() +
  theme(panel.grid = element_blank())

