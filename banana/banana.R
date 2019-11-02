
## Exploratory analysis of a banana

library(arm)
library(dplyr)
library(ggplot2)
library(readr)
library(rstan)
library(tidyr)


plot_format <- list(theme_bw(base_size = 16),
                    theme(panel.grid = element_blank()))




banana <- read_csv("banana.csv")


## Overview of the data

banana

table(banana$day, banana$slice)


day_stats <- summarise(group_by(banana, day),
                       average = mean(amyloplasts),
                       sem = sd(amyloplasts)/sqrt(length(amyloplasts)),
                       lower = average - 2 * sem,
                       upper = average + 2 * sem)

day_stats

slice_stats <- summarise(group_by(banana, day, slice),
                         average = mean(amyloplasts),
                         sem = sd(amyloplasts)/sqrt(length(amyloplasts)),
                         lower = average - 2 * sem,
                         upper = average + 2 * sem)

slice_stats



plot_day <- ggplot() +
    geom_jitter(aes(x = day,
                    y = amyloplasts),
                colour = "grey",
                data = banana) +
    geom_line(aes(x = day,
                  y = average),
              data = day_stats) +
    scale_x_continuous(breaks = c(1, 5, 9)) +
    plot_format


png(width = 500,
    height = 500,
    file = "plot_day.png")
print(plot_day)
dev.off()



plot_small_multiples <- ggplot() + 
    geom_jitter(aes(x = day,
                    y = amyloplasts),
                colour = "grey",
                data = banana) +
    geom_line(aes(x = day,
                  y = average),
              data = slice_stats) +
    facet_wrap(~ slice) +
    scale_x_continuous(breaks = c(1, 5, 9)) +
    plot_format

png(width = 750,
    height = 500,
    file = "plot_small_multiples.png")
print(plot_small_multiples)
dev.off()


## Linear model

model_lm  <- lm(amyloplasts ~ day * slice,
                data = banana)

levels <- expand.grid(slice = unique(banana$slice),
                      day = unique(banana$day),
                      stringsAsFactors = FALSE)

pred_lm  <- cbind(levels,
                  predict(model_lm,
                          newdata = levels,
                          interval = "confidence"))



## Simulate data

y_rep_lm  <- function(coef_lm, sigma, banana) {
    slice_coef  <- c(0, coef_lm[3:6])
    names(slice_coef)  <- c("A", "B", "C", "D", "E")

    slice_by_day_coef  <- c(0, coef_lm[7:10])
    names(slice_by_day_coef)  <- c("A", "B", "C", "D", "E")   

    banana$sim_amyloplasts  <- 
        coef_lm[1] +
        slice_coef[banana$slice] +
        banana$day * (coef_lm[2] + slice_by_day_coef[banana$slice]) +
        rnorm(nrow(banana), 0, sigma)
    banana
}


sim_lm  <- sim(model_lm)

sim_banana  <- y_rep_lm(sim_lm@coef[1,], sim_lm@sigma[1], banana)


plot_sim_lm <- ggplot() + 
    geom_jitter(aes(x = day,
                    y = sim_amyloplasts),
                data = sim_banana) +
    geom_jitter(aes(x = day,
                    y = amyloplasts),
                colour = "grey",
              data = sim_banana) +
    facet_wrap(~ slice) +
    scale_x_continuous(breaks = c(1, 5, 9)) +
    plot_format


png(width = 750,
    height = 500,
    file = "plot_sim_lm.png")
print(plot_sim_lm)
dev.off()



## Poisson GLM

model_glm <- glm(amyloplasts ~ day * slice,
                 data = banana,
                 family = poisson(link = log))

pred_glm <- predict(model_glm,
                    newdata = levels,
                    se.fit = TRUE)

results_glm <- data.frame(levels,
                          average = pred_glm$fit,
                          se = pred_glm$se.fit,
                          stringsAsFactors = FALSE)
  



y_rep_glm  <- function(coef_glm, banana) {
    slice_coef  <- c(0, coef_glm[3:6])
    names(slice_coef)  <- c("A", "B", "C", "D", "E")

    slice_by_day_coef  <- c(0, coef_glm[7:10])
    names(slice_by_day_coef)  <- c("A", "B", "C", "D", "E")
    

    latent  <- exp(coef_glm[1] +
        slice_coef[banana$slice] +
        banana$day * (coef_glm[2] + slice_by_day_coef[banana$slice])) 

    banana$sim_amyloplasts  <- rpois(n = nrow(banana),
                                     lambda = latent)
    banana
}


sim_glm  <- sim(model_glm, 1000)

sim_banana_glm  <- y_rep_glm(sim_glm@coef[2,], banana)


plot_sim_glm <- ggplot() + 
    geom_jitter(aes(x = day,
                    y = sim_amyloplasts),
                data = sim_banana_glm) +
    geom_jitter(aes(x = day,
                    y = amyloplasts),
                colour = "grey",
              data = sim_banana_glm) +
    facet_wrap(~ slice) +
    scale_x_continuous(breaks = c(1, 5, 9)) +
    plot_format


png(width = 750,
    height = 500,
    file = "plot_sim_glm.png")
print(plot_sim_glm)
dev.off()



## Negative binomial

model_nb  <- stan(file = "banana.stan",
                  data = list(n = nrow(banana),
                              n_slices = length(unique(banana$slice)),
                              n_days = length(unique(banana$day)),
                              amyloplasts = banana$amyloplasts,
                              day = banana$day - 1,
                              slice = as.numeric(factor(banana$slice)),
                              prior_phi_scale = 1))

model_nb2 <- stan(file = "banana.stan",
                  data = list(n = nrow(banana),
                              n_slices = length(unique(banana$slice)),
                              n_days = length(unique(banana$day)),
                              amyloplasts = banana$amyloplasts,
                              day = banana$day - 1,
                              slice = as.numeric(factor(banana$slice)),
                              prior_phi_scale = 0.1))



print(model_nb, pars = c("initial_amyloplasts", "decline", "phi_rec"))
print(model_nb2, pars = c("initial_amyloplasts", "decline", "phi_rec"))


y_rep  <- rstan::extract(model_nb, pars = "y_rep")[[1]]
y_rep2  <- rstan::extract(model_nb2, pars = "y_rep")[[1]]

sim_banana_nb  <- cbind(banana,
                        sim_amyloplasts = y_rep[1,])

sim_banana_nb2  <- cbind(banana,
                         sim_amyloplasts = y_rep2[1,])


plot_sim_nb <- ggplot() + 
    geom_jitter(aes(x = day,
                    y = sim_amyloplasts),
                data = sim_banana_nb) +
    geom_jitter(aes(x = day,
                    y = amyloplasts),
                colour = "grey",
              data = sim_banana_nb) +
    facet_wrap(~ slice) +
    scale_x_continuous(breaks = c(1, 5, 9)) +
    plot_format

plot_sim_nb2 <- ggplot() + 
    geom_jitter(aes(x = day,
                    y = sim_amyloplasts),
                data = sim_banana_nb2) +
    geom_jitter(aes(x = day,
                    y = amyloplasts),
                colour = "grey",
              data = sim_banana_nb2) +
    facet_wrap(~ slice) +
    scale_x_continuous(breaks = c(1, 5, 9)) +
    plot_format


png(width = 750,
    height = 500,
    file = "plot_sim_nb.png")
print(plot_sim_nb)
dev.off()


png(width = 750,
    height = 500,
    file = "plot_sim_nb2.png")
print(plot_sim_nb2)
dev.off()



### PPC

### Count number of 0s
### Find maximum value
### Look at variance


check_glm  <- data.frame(n_zeros = numeric(1000),
                         max_value = numeric(1000),
                         variance = numeric(1000),
                         model = "Poisson",
                         stringsAsFactors = FALSE)

check_nb  <- data.frame(n_zeros = numeric(1000),
                        max_value = numeric(1000),
                        variance = numeric(1000),
                        model = "Negative binomial",
                        stringsAsFactors = FALSE)

check_nb2  <- data.frame(n_zeros = numeric(1000),
                         max_value = numeric(1000),
                         variance = numeric(1000),
                         model = "Negative binomial 2",
                         stringsAsFactors = FALSE)


for (sim_ix in 1:1000) {
    y_rep_data  <- y_rep_glm(sim_glm@coef[sim_ix,], banana)
    check_glm$n_zeros[sim_ix]  <- sum(y_rep_data$sim_amyloplasts == 0)
    check_glm$max_value[sim_ix] <- max(y_rep_data$sim_amyloplasts)
    check_glm$variance[sim_ix] <- var(y_rep_data$sim_amyloplasts)

    check_nb$n_zeros[sim_ix]  <- sum(y_rep[sim_ix,] == 0)
    check_nb$max_value[sim_ix]  <- max(y_rep[sim_ix,])
    check_nb$variance[sim_ix]  <- var(y_rep[sim_ix,])

    check_nb2$n_zeros[sim_ix]  <- sum(y_rep2[sim_ix,] == 0)
    check_nb2$max_value[sim_ix]  <- max(y_rep2[sim_ix,])
    check_nb2$variance[sim_ix]  <- var(y_rep2[sim_ix,])
}

check  <- rbind(check_glm,
                check_nb,
                check_nb2)

melted_check  <- gather(check, "variable", "value", -model)

check_data  <- data.frame(n_zeros = sum(banana$amyloplasts == 0),
                          max_value = max(banana$amyloplasts),
                          variance = var(banana$amyloplasts))

melted_check_data  <- gather(check_data, "variable", "value")

melted_data_check  <-  cbind(melted_check_data,
                             model = rep(c("Poisson",
                                           "Negative binomial",
                                           "Negative binomial 2"), each = 3))

plot_check  <- qplot(x = value, data = melted_check) +
    facet_wrap(model ~ variable, scale = "free") +
    geom_vline(aes(xintercept = value), data = melted_data_check) +
    plot_format


png(width = 750,
    height = 500,
    file = "plot_check.png")
print(plot_check)
dev.off()


## Combine fits to plot all models together

results_glm$lwr  <- exp(results_glm$average - 2 * results_glm$se)
results_glm$upr  <- exp(results_glm$average + 2 * results_glm$se)
results_glm$fit  <- exp(results_glm$average)



pred_stan <- function(model, newdata) {
    samples <- rstan::extract(model)
    initial_amyloplasts <- data.frame(samples$initial_amyloplasts)
    decline  <- data.frame(samples$decline)
    names(initial_amyloplasts) <- names(decline) <- c("A", "B", "C", "D", "E")

    ## Get posterior for levels
    pred  <- matrix(0,
                    ncol = nrow(newdata),
                    nrow = nrow(initial_amyloplasts))

    for (obs in 1:ncol(pred)) {
        pred[,obs]  <- initial_amyloplasts[,newdata$slice[obs]] +
            (newdata$day[obs] - 1) * decline[,newdata$slice[obs]]
    }

    ## Get mean and interval
    newdata$fit  <- exp(colMeans(pred))
    intervals <- lapply(data.frame(pred), quantile, probs = c(0.025, 0.975))
    newdata$lwr  <- exp(unlist(lapply(intervals, "[", 1)))
    newdata$upr  <- exp(unlist(lapply(intervals, "[", 2)))

    newdata
}

pred_lm$model  <- "Linear model"

pred_nb <- pred_stan(model_nb, levels)
pred_nb2 <- pred_stan(model_nb2, levels)

results_glm$model <- "Poisson"
pred_nb$model <- "Negative binomial"
pred_nb2$model <- "Negative binomial 2"

results_combined  <- rbind(results_glm[, c("slice", "day", "fit", "lwr", "upr", "model")],
                           pred_nb,
                           pred_nb2,
                           pred_lm)

plot_models <- ggplot() +
    geom_jitter(aes(x = day,
                    y = amyloplasts),
                colour = "grey",
                data = banana) +
    geom_line(aes(x = day,
                  y = fit,
                  colour = model),
              data = results_combined) +
    geom_pointrange(aes(x = day,
                        ymin = lwr,
                        ymax = upr,
                        y = fit,
                        colour = model),
                    position = position_dodge(width = 1),
                    data = results_combined) +
    facet_wrap(~ slice) +
    scale_x_continuous(breaks = c(1, 5, 9)) +
    plot_format
