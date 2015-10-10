library(rstan)

families <- 100
offspring <- 2

family_sigma <- 0.5
residual_sigma <- 1

family <- unlist(lapply(1:families, rep, times = offspring))
family_effect <- rnorm(families, 0, family_sigma)

data <- data.frame(id = 1:(families*offspring),
                   family,
                   response =  family_effect[family] +
                     rnorm(families*offspring, 0, residual_sigma))

stan_data <- list(N = families*offspring,
                  families = families,
                  response = data$response,
                  family = data$family)

model <- stan(model_code = "
     data {
       int <lower = 0> N;
       int <lower = 0> families;
       vector[N] response;
       int family[N];
     }
     parameters {
       vector[families] family_effect;
       real intercept;
       real <lower = 0> residual_sigma;
       real <lower = 0> family_sigma;
     }
     model {
       // family_sigma ~ cauchy(0, 25);
       family_effect ~ normal(0, family_sigma);
       for (i in 1:N)
         response[i] ~ normal(intercept + family_effect[family[i]], residual_sigma);
     }
     ",
     data = stan_data,
     iter = 1000, chains = 4)


model_lm <- lm(response ~ -1 + factor(family), data)
coef(model_lm)
confint(model_lm)
summary(model_lm)$sigma

print(model)

cor(get_posterior_mean(model)[1:families, 5], family_effect)
cor(get_posterior_mean(model)[1:families, 5], coef(model_lm))
